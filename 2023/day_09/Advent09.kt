package advent2023.day09

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }
        .map { line -> line.split(" ").map { it.toLong() } }
        .toList()

    println("Part 1: ${Solution(input).part1()}")
    println("Part 2: ${Solution(input).part2()}")
}

class Solution(val input: List<List<Long>>) {
    val m = input[0].size

    fun part1(): Long = input.indices.sumOf { calcNext(it) }
    fun part2(): Long = input.indices.sumOf { calcPrev(it) }

    private fun calcNext(i: Int): Long = diffTable(i).debug { "DT[$i] = $it" }.extrapolateRight()
    private fun calcPrev(i: Int): Long = diffTable(i).extrapolateLeft()

    private fun diffTable(i: Int): DiffTable =
        generateSequence(input[i].debug()) { it.zipWithNext { prv, nxt -> nxt - prv }.debug() }
            .takeWhile { row -> !row.all { it == 0L } } // won't return the trailing row of zeros, it's "implied"
            .toList()

    private fun DiffTable.extrapolateRight(): Long = extrapolateRight(0, m)
    private fun DiffTable.extrapolateLeft(): Long = extrapolateLeft(0, -1)

    private fun DiffTable.extrapolateLeft(i: Int, j: Int): Long = when {
        i == size -> 0L
        i in indices && j in get(i).indices -> get(i).get(j)
        else -> extrapolateLeft(i, j + 1) - extrapolateLeft(i + 1, j)
    }

    private fun DiffTable.extrapolateRight(i: Int, j: Int): Long = when {
        i == size -> 0L
        i in indices && j in get(i).indices -> get(i).get(j)
        else -> extrapolateRight(i, j - 1) + extrapolateRight(i + 1, j - 1)
    }
}

typealias DiffTable = List<List<Long>>

