package advent2023.day19

import kotlin.math.*
import java.util.SortedSet

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val (items, workflows) = generateSequence { readlnOrNull()?.trimEnd() }
        .filter { it.isNotBlank() }
        .partition { it.startsWith("{") }
        .run { first.map { it.asItem() } to second.map { it.asWorkflow() }.associateBy { it.name } }

    items.filter { it.isAcceptedBy(workflows) }
        .sumOf { it.x + it.m + it.a + it.s }
        .also { println("Part 1: $it") }

    println("Part 2: ${countDistinctAcceptedItems(workflows)}")
}

fun Item.isAcceptedBy(wfs: Workflows): Boolean {
    var wfId = "in"
    while (wfId !in listOf("A", "R")) {
        val wf = wfs[wfId] ?: error("unknown workflow: $wfId")
        wfId = wf.branches.first { it.condition(this) }.goto
    }
    return wfId == "A"
}

val dimensions = "xmas"


data class HyperRange(
    val axes: Map<Char, Range1> = dimensions.associateWith { Range1() }
) {
    val volume get() = axes.values.map { it.length }.product()
    fun partition(cmd: String): Pair<HyperRange, HyperRange> {
        val dim = cmd[0]
        val pivot = cmd.parseInts()[0]
        return axes[dim]!!.let {
            when(cmd[1]) {
                '>' -> { it.partitionGreaterThan(pivot) }
                '<' -> { it.partitionLessThan(pivot) }
                else -> error("unknown operation ${cmd[1]} in $cmd")
            }
        }.let { ranges -> ranges.map { it -> modify(dim, it) } }
    }

    private fun modify(dim: Char, newRange: Range1) = HyperRange(
        axes.toMutableMap().also { it[dim] = newRange }
    )
}

fun <T, R> Pair<T, T>.map(func: (T) -> R): Pair<R, R> = Pair(func(first), func(second))
fun Iterable<Long>.product() = fold(1L) { acc, it -> acc * it }

data class Range1(val min: Val = 1, val max: Val = 4000) {
    fun partitionGreaterThan(pivot: Val) = Pair(coerceGreaterThan(pivot), coerceLessThan(pivot + 1))
    fun partitionLessThan(pivot: Val) = Pair(coerceLessThan(pivot), coerceGreaterThan(pivot - 1))

    fun coerceGreaterThan(pivot: Val) = Range1(maxOf(min, pivot + 1), maxOf(max, pivot + 1))
    fun coerceLessThan(pivot: Val) = Range1(minOf(min, pivot - 1), minOf(max, pivot - 1))

    val length = maxOf(0, max - min + 1).toLong()
}

fun countDistinctAcceptedItems(wfs: Workflows): Long {
    var count = 0L
    val todo = ArrayDeque<State>().apply { add(State("in", HyperRange())) }

    while (todo.isNotEmpty()) {
        val (wfName, cube) = todo.removeFirst()
        if (wfName == "A") count += cube.volume
        if (wfName in "AR") continue
        val wf = wfs[wfName]!!

        wf.branches.map { it.src }.fold(cube) { accCube, cmd ->
            if (":" in cmd) {
                val (ifYes, ifNot) = accCube.partition(cmd)
                todo.addLast(State(cmd.substringAfter(":"), ifYes))
                ifNot
            } else {
                todo.addLast(State(cmd, accCube))
                accCube
            }
        }
    }

    return count
}

data class State(val wfName: String, val cube: HyperRange)

fun String.asItem() = parseInts().run { Item(get(0), get(1), get(2), get(3)) }
fun String.parseInts() = intsRegex.findAll(this).map { it.value.toInt() }.toList()
val intsRegex = "\\d+".toRegex()

fun String.asWorkflow() = split("[{}]".toRegex()).run {
    Workflow(get(0), get(1).split(",").map { it.asBranch() })
}
fun String.asBranch() = when {
    ":" !in this -> Branch({ true }, this, this)
    else -> {
        check("<" in this || ">" in this) { "cannot parse branch: $this" }
        val constant = parseInts()[0]
        val getProp = first().asGetter()
        Branch(
            condition = {
                val cmp = it.getProp().compareTo(constant)
                if ("<" in this) cmp < 0 else cmp > 0
            },
            goto = substringAfter(":"),
            src = this,
        )
    }
}
fun Char.asGetter(): Item.() -> Val = when(this) {
    'x' -> then { x }
    'm' -> then { m }
    'a' -> then { a }
    's' -> then { s }
    else -> error("unknown prop: " + this)
}
fun <T> then(x: T): T = x

data class Item(val x: Val, val m: Val, val a: Val, val s: Val)
typealias Val = Int

typealias Workflows = Map<String, Workflow>
class Workflow(val name: String, val branches: List<Branch>)
class Branch(val condition: Condition, val goto: String, val src: String)
typealias Condition = (Item) -> Boolean

