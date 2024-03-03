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
        .sumOf { it.values.sum() }
        .also { println("Part 1: $it") }

    println("Part 2: ${countDistinctAcceptedItems(workflows)}")
}

fun String.asItem() = dimensions.toList().zip(parseInts()).toMap()
fun String.parseInts() = intsRegex.findAll(this).map { it.value.toInt() }.toList()
val intsRegex = "\\d+".toRegex()

fun String.asWorkflow() = split("[{}]".toRegex()).run { Workflow(get(0), get(1).split(",")) }

typealias Item = Map<Char, Val>
typealias Val = Int

typealias Workflows = Map<String, Workflow>
class Workflow(val name: String, val branches: List<Branch>)
typealias Branch = String
val Branch.goto get() = substringAfter(":")

fun Item.isAcceptedBy(wfs: Workflows): Boolean {
    var wfId = "in"
    while (wfId !in listOf("A", "R")) {
        val wf = wfs[wfId] ?: error("unknown workflow: $wfId")
        wfId = wf.branches.first { branch ->
            when {
                ":" !in branch -> true
                else -> {
                    val prop = get(branch[0])!!
                    val v = branch.parseInts()[0]
                    val cmp = prop.compareTo(v)
                    if (">" in branch) cmp > 0 else cmp < 0
                }
            }
        }.goto
    }
    return wfId == "A"
}

fun countDistinctAcceptedItems(wfs: Workflows): Long {
    var count = 0L
    val todo = ArrayDeque<State>().apply { add(State("in", HyperRange())) }

    while (todo.isNotEmpty()) {
        val (wfName, cube) = todo.removeFirst()
        if (wfName == "A") count += cube.volume
        if (wfName in "AR") continue
        val wf = wfs[wfName]!!

        wf.branches.fold(cube) { accCube, branch ->
            if (":" in branch) {
                val (ifYes, ifNot) = accCube.partition(branch)
                todo.addLast(State(branch.goto, ifYes))
                ifNot
            } else {
                todo.addLast(State(branch, accCube))
                accCube
            }
        }
    }

    return count
}

val dimensions = "xmas"

data class HyperRange(
    val axes: Map<Char, Range1> = dimensions.associateWith { Range1() }
) {
    val volume get() = axes.values.map { it.length }.product()

    fun partition(branch: String): Pair<HyperRange, HyperRange> {
        val dim = branch[0]
        val pivot = branch.parseInts()[0]
        return axes[dim]!!.let {
            when(branch[1]) {
                '>' -> { it.partitionGreaterThan(pivot) }
                '<' -> { it.partitionLessThan(pivot) }
                else -> error("unknown operation ${branch[1]} in $branch")
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

data class State(val wfName: String, val cube: HyperRange)

