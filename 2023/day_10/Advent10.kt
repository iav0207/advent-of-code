package advent2023.day10

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

typealias Field = List<String>
operator fun Field.contains(c: Coord) = c.i in indices && c.j in get(0).indices

data class Coord(val i: Int, val j: Int)
operator fun Coord.plus(d: Direction) = Coord(i + d.di, j + d.dj)

data class Direction(val di: Int, val dj: Int)

class Solution(private val field: Field) {
    private val startPos: Coord = field.mapIndexed { i, line -> i to line.indexOfFirst { it == 'S' } }
        .find { it.second != -1 }!!
        .run { Coord(first, second) }
        .debug { "start at $it" }

    private val startShape: Char = nsew.map { direc -> direc to (startPos + direc) }
        .filter { (_, neighbor) -> neighbor in field }
        .map { (direc, neighbor) -> direc to next[shapeAt(neighbor)]!!.map { neighbor + it } }
        .filter { startPos in it.second }
        .map { it.first }.toSet()
        .let { direcs -> next.entries.find { it.value == direcs }!!.key }
        .debug { "start shape: $it" }

    fun part1(): Int = loop().size.div(2)
    fun part2(): Int = enclosedBy(loop()).size

    private fun loop(): Set<Coord> = mutableSetOf<Coord>().apply {
        val todo = ArrayDeque<Coord>().apply { add(startPos) }
        while (todo.isNotEmpty()) {
            followThePipe(todo.removeFirst())
                .filter { add(it) }
                .forEach { todo.add(it) }
        }
    }

    /** Cutting the field with side diagonals, odd number of loop cuts means an enclosed tile. */
    private fun enclosedBy(loop: Set<Coord>): Set<Coord> = mutableSetOf<Coord>().apply {
        val cuts = MutableList(field.size + field[0].length) { 0 }
        coords().forEach { c ->
            if (c in loop && shapeAt(c).cutsSideDiagonal()) cuts[c.i + c.j]++
            else if (c !in loop && cuts[c.i + c.j].isOdd()) add(c)
        }
    }.debug { draw(loop, it) }

    private fun Char.cutsSideDiagonal() = this in "|-L7"
    fun Int.isOdd() = mod(2) == 1

    private fun draw(loop: Set<Coord>, enclosed: Set<Coord>): String = coords().map { c -> when {
        c in loop -> shapeAt(c)
        c in enclosed -> '■'
        else -> '□'
    }}.joinToString("").chunked(field[0].length).joinToString("\n")

    private fun coords(): Sequence<Coord> = field.indices.asSequence()
        .flatMap { i -> field[i].indices.asSequence().map { j -> i to j } }
        .map { Coord(it.first, it.second) }

    private fun followThePipe(c: Coord): Iterable<Coord> = next[shapeAt(c)]!!.map { c + it }.filter { it in field }
    private fun shapeAt(c: Coord): Char = if (c == startPos) startShape else c.run { field[i][j] }
    private fun around(c: Coord): Iterable<Coord> = nsew.map { c + it }.filter { it in field }

    companion object {
        val north = Direction(-1, 0)
        val south = Direction(1, 0)
        val east = Direction(0, 1)
        val west = Direction(0, -1)

        val next = mapOf<Char, Set<Direction>>(
            '|' to setOf(north, south),
            '-' to setOf(east, west),
            'L' to setOf(north, east),
            'J' to setOf(north, west),
            '7' to setOf(south, west),
            'F' to setOf(south, east),
            '.' to setOf(),
        )

        val nsew = listOf(north, south, east, west)
    }
}

