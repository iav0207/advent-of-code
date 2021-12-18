package advent2021.day18

import java.util.*
import kotlin.math.ceil


var debug = false
fun debug(a: Any) = if (debug) println(a) else Unit
fun debug(a: () -> Any) = if (debug) println(a()) else Unit


fun main(args: Array<String>) {
    args.find { it == "-d" }?.also { debug = true }

    val rows = generateSequence { readlnOrNull() }.map { parse(it) }
    rows
        .map { ArrayList(it) as MutableList<Value> }
        .reduce { prev, next -> process(prev + next) }
        .also { debug { it } }
        .also { println("Magnitude of the addition result is ${it.magnitude()}") }

}

fun parse(line: String): MutableList<Value> {
    var depth = 0
    val nums = line.replace(Regex("\\D+"), ",").split(",").filter { it.isNotBlank() }.map { it.toInt() }
    val numQ : Iterator<Int> = nums.iterator()
    val stx : String = line.replace(Regex("\\d+"), "")
    val values: MutableList<Value> = stx.zipWithNext { prev, next ->
        if (prev == '[') {
            depth += 1
            if (next == ',') return@zipWithNext Value(depth, numQ.next())
        }
        if (prev == ',' && next == ']') return@zipWithNext Value(depth, numQ.next())
        if (prev == ']') { depth -= 1 }
        check(depth > 0)
        null
    }.filterNotNull().toMutableList()
    check(values.size == nums.size)
    return values
}

fun process(row: MutableList<Value>): MutableList<Value> {
    var changed: Boolean
    do {
        var i = 0
        changed = false
        debug { row }
        while (i in row.indices) {
            val v = row[i]
            if (v.depth > 4) {
                debug { "Explosion at index $i" }
                check(row[i].depth == row[i + 1].depth)
                val explodingPair = v to row[i + 1]
                if (i > 0) row[i - 1].value += explodingPair.first.value
                if (i + 2 < row.size) row[i + 2].value += explodingPair.second.value
                row[i].depth--
                row[i].value = 0
                row.removeAt(i + 1)
                changed = true
                break
            }
            i++
        }
        if (changed) continue
        i = 0
        while (i in row.indices) {
            val v = row[i]
            if (v.value > 9) {
                debug { "Split at index $i" }
                val left = Value(v.depth + 1, v.value.halfDown())
                val right = Value(v.depth + 1, v.value.halfUp())
                row[i] = left
                if (i + 1 in row.indices) row.add(i + 1, right)
                else row.add(right)
                changed = true
                break
            }
            i++
        }
//        if (changed) break
        i++
    } while (changed)
    return row
}

operator fun List<Value>.plus(o: List<Value>) = MutableList(size + o.size) {
    val item = if (it in indices) get(it) else o[it - size]
    item.apply { depth++ }
}

fun List<Value>.magnitude(): Int {
    val maxDepth: Int = maxOfOrNull { it.depth }!!
//    val nodesByDepth = List<MutableList<Tree>>(5) { mutableListOf() }
    val vStack = Stack<IndexedValue<Value>>()
    var result: Int = 0
    for (i in indices) {
        if (vStack.isEmpty() || vStack.peek().value.depth != get(i).depth) {
            vStack.push(IndexedValue(i, get(i)))
            continue
        }
        var depth = get(i).depth
        var rightVal: Int = get(i).value
//        var subResult: Int = get(i).value
        while (vStack.isNotEmpty() && vStack.peek().value.depth == depth) {
            val left = vStack.pop()
            rightVal = 3 * left.value.value + 2 * rightVal
            depth--
        }
        vStack.push(IndexedValue(-1, Value(depth, rightVal)))
        result = rightVal
    }
    return result
//    for (d in maxDepth downTo 0) {
//        withIndex().filter { it.value.depth == d }
//    }
}

class Tree(val v: Value, var left: Tree? = null, var right: Tree? = null)

fun Int.halfDown(): Int = floorDiv(2)
fun Int.halfUp(): Int = ceil(toDouble().div(2)).toInt()

class Value(var depth: Int, var value: Int) {
    override fun toString() = "Value($value at $depth)"
}


