package advent2023.day21

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }.toList()
    Solution(input).apply {
        println("Part 1: ${part1()}")
    }
}

class Solution(private val field: List<String>) {
    private val start = field.indices.flatMap { i -> field[i].indices.map { j -> Vec(i, j) } }.find { field[it] == 'S' }!!

    fun part1(): Int = generateSequence(Step(setOf(start))) { it.further() }
        .take(1 + 64)
        .last().positions.size

    inner class Step(val positions: Set<Vec>) {
        fun further(): Step = positions.flatMap { p -> directions.map { p + it } }
            .filter { it in field && field[it] != '#' }
            .run { Step(toSet()) }

        private val directions = listOf(Vec(1, 0), Vec(0, 1), Vec(-1, 0), Vec(0, -1))
    }

    operator fun List<String>.contains(v: Vec) = v.i in field.indices && v.j in field[0].indices
    operator fun List<String>.get(v: Vec) = field[v.i][v.j]
}

data class Vec(val i: Int, val j: Int)
operator fun Vec.plus(o: Vec) = Vec(i + o.i, j + o.j)
