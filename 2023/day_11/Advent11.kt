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
    }
}

data class Coord(val i: Int, val j: Int)

class Solution(val field: List<String>) {
    val n = field.size
    val m = field[0].length
    val emptyRows = field.withIndex()
        .filter { (_, line) -> '#' !in line }
        .map { it.index }
        .debug { "empty rows: $it" }

    val emptyCols = (0 until m)
        .filter { j -> (0 until n).none { i -> field[i][j] == '#' } }
        .debug { "empty columns: $it"}

    val galaxies = coords().filter { at(it) == '#' }.toList()

    fun part1() = galaxyPairs().sumOf { gp -> distance(gp.first, gp.second).debug { "dist($gp) = $it" } }

    fun distance(g1i: Int, g2i: Int): Int {
        val (g1, g2) = galaxies[g1i] to galaxies[g2i]
        val iMin = minOf(g1.i, g2.i)
        val iMax = maxOf(g1.i, g2.i)
        val jMin = minOf(g1.j, g2.j)
        val jMax = maxOf(g1.j, g2.j)
        return abs(g1.i - g2.i) + abs(g1.j - g2.j) + emptyCols.count { it in (jMin+1 until jMax) } + emptyRows.count { it in (iMin+1 until iMax) }
    }

    fun galaxyPairs(): Sequence<Pair<Int, Int>> = galaxies.indices.asSequence().flatMap { gi ->
        galaxies.indices.asSequence().filter { it > gi }.map { gi to it }
    }

    fun at(c: Coord) = field[c.i][c.j]
    fun coords() = field.indices.asSequence().flatMap {
        i -> field[0].indices.asSequence().map { j -> Coord(i, j) }
    }
}

