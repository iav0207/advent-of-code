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
    println("Part 2: ${Solution(input).part2()}")
}

class Solution(val input: List<MutableList<Long>>) {
    val n = input.size
    val m = input[0].size

    fun part1(): Long = input.indices.sumOf { calcNext(it) }
    fun part2(): Long = input.indices.sumOf { calcPrev(it).debug { "LEFT $it" } }

    fun calcNext(i: Int): Long = diffTable(i).debug { "DT[$i] = $it" }.extrapolateRight()
    fun calcPrev(i: Int): Long = diffTable(i).extrapolateLeft()

    fun diffTable(i: Int): DiffTable =
        generateSequence(input[i].debug()) { it.zipWithNext { prv, nxt -> nxt - prv }.toMutableList().debug() }
            .takeWhile { row -> !row.all { it == 0L } } // won't return trailing row of zeros
            .toList()

    fun DiffTable.extrapolateRight(): Long = extrapolateRight(0, m)
    fun DiffTable.extrapolateLeft(): Long = extrapolateLeft(0, -1)

    fun DiffTable.extrapolateLeft(i: Int, j: Int): Long = run {
        debug { "extr $i $j" }
        when {
            i == size -> 0L
            i in indices && j in get(i).indices -> get(i).get(j)
            else -> extrapolateLeft(i, j + 1) - extrapolateLeft(i + 1, j)
        }
    }

    fun DiffTable.extrapolateRight(i: Int, j: Int): Long {
        debug { "extr $i $j" }
        if (i == size) return 0L
        if (i in indices && j in get(i).indices) return get(i).get(j)
        return extrapolateRight(i, j - 1) + extrapolateRight(i + 1, j - 1)
    }
}

typealias DiffTable = List<MutableList<Long>>

