package advent2023.day10

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val field = generateSequence { readlnOrNull()?.trimEnd() }.toList()

    Solution(field).run {
        println("Part 1: ${part1()}")
    }
}

class Solution(private val field: List<String>) {
    val startPos: Coord = field.mapIndexed { i, line -> i to line.indexOfFirst { it == 'S' } }
        .find { it.second != -1 }!!
        .run { Coord(first, second) }
        .debug { "start at $it" }

    val startShape: Char = nsew.map { direc -> direc to (startPos + direc) }
        .filter { (_, neighbor) -> neighbor in field }
        .map { (direc, neighbor) -> direc to next[at(neighbor)]!!.map { neighbor + it } }
        .filter { startPos in it.second }
        .map { it.first }.toSet()
        .let { direcs -> next.entries.find { it.value == direcs }!!.key }
        .debug { "start shape: $it" }

    fun part1(): Int = loop().size.div(2).toInt()
    fun loop(): Set<Coord> = mutableSetOf<Coord>().apply {
        val todo = ArrayDeque<Coord>().apply { add(startPos) }
        while (todo.isNotEmpty()) {
            next(todo.removeFirst())
                .filter { add(it) }
                .forEach { todo.add(it) }
        }
    }

    private fun next(c: Coord): Iterable<Coord> {
        val shape = if (c == startPos) startShape else at(c)
        return next[shape]!!.map { c + it }.filter { it in field }
    }

    private fun around(c: Coord): Iterable<Coord> = nsew.map { c + it }.filter { it in field }
    private fun at(c: Coord): Char = c.run { field[i][j] }

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

typealias Field = List<String>
operator fun Field.contains(c: Coord) = c.i in indices && c.j in get(0).indices

data class Coord(val i: Int, val j: Int)
operator fun Coord.plus(d: Direction) = Coord(i + d.di, j + d.dj)

data class Direction(val di: Int, val dj: Int)
fun Direction.inv() = Direction(-di, -dj)

