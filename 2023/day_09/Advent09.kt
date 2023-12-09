package advent2023.day09

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }
        .map { line -> line.split(" ").map { it.toLong() }.toMutableList() }
        .toList()

    println("Part 1: ${Solution(input).part1()}")
}

class Solution(val input: List<MutableList<Long>>) {
    val n = input.size
    val m = input[0].size

    fun part1(): Long = input.indices.sumOf { calcNext(it) }

    fun diffTable(i: Int): DiffTable =
        generateSequence(input[i].debug()) { .zipWithNext { prv, nxt -> nxt - prv }.toMutableList().debug() }
            .takeWhile { row -> !row.all { it == 0L } } // won't return trailing row of zeros
            .toList()

    fun DiffTable.extrapolate(): Long = extrapolate(0, m)

    fun DiffTable.extrapolate(i: Int, j: Int): Long {
        debug { "extr $i $j" }
        if (i == size) return 0L
        if (i in indices && j in get(i).indices) return get(i).get(j)
        return extrapolate(i, j - 1) + extrapolate(i + 1, j - 1)
    }

    fun calcNext(i: Int): Long = diffTable(i).debug { "DT[$i] = $it" }.extrapolate()
}

typealias DiffTable = List<MutableList<Long>>

