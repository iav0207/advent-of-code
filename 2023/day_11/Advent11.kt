package advent2023.day11

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val field = generateSequence { readlnOrNull()?.trimEnd() }.toList()

    Solution(field).apply {
        println("Part 1: ${part1()}")
        println("Part 2: ${part2()}")
    }
}

data class Coord(val i: Int, val j: Int)

val galaxy = '#'

class Solution(val field: List<String>) {
    val n = field.size
    val m = field[0].length
    val emptyRows = field.withIndex()
        .filter { (_, line) -> galaxy !in line }
        .map { it.index }
        .debug { "empty rows: $it" }

    val emptyCols = (0 until m)
        .filter { j -> (0 until n).none { i -> field[i][j] == galaxy } }
        .debug { "empty columns: $it"}

    val galaxies = coords().filter { at(it) == galaxy }.toList().debug { "galaxies count: ${it.size} n = $n m = $m" }

    fun part1() = galaxyPairs().sumOf { distance(it) }
    fun part2() = galaxyPairs().sumOf { distance(it, emptySpaceMultiplier = 1_000_000L) }

    fun distance(gPair: Pair<Int, Int>, emptySpaceMultiplier: Long = 2L): Long {
        val (c1, c2) = galaxies[gPair.first] to galaxies[gPair.second]
        return manhattan(c1, c2) + emptySpaceBetween(c1, c2) * (emptySpaceMultiplier - 1)
    }

    fun manhattan(c1: Coord, c2: Coord) = abs(c1.i - c2.i) + abs(c1.j - c2.j)

    fun emptySpaceBetween(c1: Coord, c2: Coord): Long =
        emptyCols.count { it in (minOf(c1.j, c2.j) + 1 until maxOf(c1.j, c2.j)) }
            .toLong()
            .plus(emptyRows.count { it in (minOf(c1.i, c2.i) until maxOf(c1.i, c2.i)) })

    fun galaxyPairs(): Sequence<Pair<Int, Int>> = galaxies.indices.asSequence().flatMap { gi ->
        galaxies.indices.asSequence().filter { it > gi }.map { gi to it }
    }

    fun at(c: Coord) = field[c.i][c.j]
    fun coords() = field.indices.asSequence().flatMap {
        i -> field[0].indices.asSequence().map { j -> Coord(i, j) }
    }
}

