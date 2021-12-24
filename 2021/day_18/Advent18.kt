package advent2021.day18

import java.util.*
import kotlin.math.ceil


var debug = false
fun debug(a: Any) = if (debug) println(a) else Unit
fun debug(a: () -> Any) = if (debug) println(a()) else Unit


/**
 * #notrees yay!
 */
fun main(args: Array<String>) {
    debug = "-d" in args

    val rows = generateSequence { readlnOrNull() }.map { parse(it) }.toList()

    rows
        .reduce { prev, next -> process(prev + next) }
        .also { debug { it } }
        .also { println("Magnitude of the addition result is ${it.magnitude()}") } // part 1

    rows.indices.maxOf { i ->
        rows.indices.filter { it != i }.maxOf { j ->
            process(rows[i] + rows[j]).magnitude()
        }
    }.also { maxMagnitude -> println("Maximum possible magnitude of a pair addition is $maxMagnitude") } // part 2
}

typealias Row = MutableList<Item>

data class Item(var depth: Int, var value: Int)

operator fun Row.plus(o: Row) = MutableList(size + o.size) {
    val item = if (it in indices) get(it) else o[it - size]
    item.copy(depth = item.depth + 1)
}

fun parse(line: String): Row {
    val values = line.replace(Regex("\\D+"), " ").split(" ").filter { it.isNotBlank() }.map { it.toInt() }
    val valueSeq: Iterator<Int> = values.iterator()
    val delimiters: String = line.replace(Regex("\\d+"), "")

    var depth = 0
    return delimiters.zipWithNext { prev, next ->
        if (prev == '[') depth++
        if (prev == ']') depth--
        check(depth > 0)
        when {
            prev == '[' && next == ',' -> Item(depth, valueSeq.next())
            prev == ',' && next == ']' -> Item(depth, valueSeq.next())
            else -> null
        }
    }.filterNotNull()
        .toMutableList()
        .apply { check(size == values.size) }
}

fun process(row: Row): Row = row.apply {
    var changed: Boolean
    do {
        changed = false
        debug { this }

        withIndex()
            .find { it.value.depth > 4 }
            ?.also { explodeAt(it.index) }
            ?.also { changed = true }

        if (changed) continue

        withIndex()
            .find { it.value.value > 9 }
            ?.also { row.splitAt(it.index) }
            ?.also { changed = true }
    } while (changed)
}

fun Row.explodeAt(i: Int) {
    debug { "Explosion at index $i" }
    val exploded = get(i) to get(i + 1)
    check(exploded.first.depth == exploded.second.depth)
    if (i - 1 in indices) get(i - 1).value += exploded.first.value
    if (i + 2 in indices) get(i + 2).value += exploded.second.value
    get(i).depth--
    get(i).value = 0
    removeAt(i + 1)
}

fun Row.splitAt(i: Int) {
    debug { "Split at index $i" }
    val it = get(i)
    val left = Item(it.depth + 1, it.value.halfDown())
    val right = Item(it.depth + 1, it.value.halfUp())
    set(i, left)
    if (i + 1 in indices) add(i + 1, right) else add(right)
}

fun Int.halfDown(): Int = floorDiv(2)
fun Int.halfUp(): Int = ceil(toDouble().div(2)).toInt()

fun Row.magnitude(): Int {
    val stack = Stack<Item>()
    forEach {
        if (stack.isEmpty() || stack.peek().depth != it.depth) {
            stack.push(it)
            return@forEach
        }
        var depth = it.depth
        var rightVal: Int = it.value
        while (stack.isNotEmpty() && stack.peek().depth == depth) {
            val left = stack.pop()
            rightVal = 3 * left.value + 2 * rightVal
            depth--
        }
        stack.push(Item(depth, rightVal))
    }
    return stack.pop().value
}
