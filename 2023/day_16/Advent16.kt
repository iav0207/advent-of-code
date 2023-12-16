package advent2023.day16

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }.toList()

    Solution(input).apply {
        println("Part 1: ${part1()}")
        println("Part 2: ${part2()}")
    }
}

typealias Field = List<String>

data class Coord(val i: Int, val j: Int)
operator fun Coord.plus(o: Coord) = Coord(i + o.i, j + o.j)

typealias Direction = Coord
val N = Direction(-1, 0)
val S = Direction(1, 0)
val E = Direction(0, 1)
val W = Direction(0, -1)
fun Direction.isVertical() = this == N || this == S
fun Direction.isHorizontal() = this == E || this == W

data class Ray(val pos: Coord, val direc: Direction)

class Solution(private val field: Field) {
    private val n = field.size
    private val m = field[0].length

    fun part1(): Int = energizedTiles().debug { it.energyLayout() }.size

    fun part2(): Int = everyRowNum.map { i -> Ray(Coord(i, 0), E) }
        .plus(everyRowNum.map { i -> Ray(Coord(i, m - 1), W) })
        .plus(everyColNum.map { j -> Ray(Coord(0, j), S) })
        .plus(everyColNum.map { j -> Ray(Coord(n - 1, j), N) })
        .maxOf { start -> energizedTiles(start).size }

    private val everyRowNum get() = field.indices.asSequence()
    private val everyColNum get() = field[0].indices.asSequence()

    private fun energizedTiles(start: Ray = Ray(Coord(0, 0), E)): Set<Coord> = mutableSetOf<Coord>().apply {
        val visited = mutableSetOf<Ray>()
        val queue = ArrayDeque<Ray>().apply { add(start) }

        while (queue.isNotEmpty()) {
            val ray = queue.removeFirst()
            if (!visited.add(ray)) continue

            add(ray.pos)
            ray.children().forEach { queue.add(it) }
        }
    }

    private fun Ray.children(): List<Ray> = when(get(pos)) {
        '|' -> when(direc) {
            E, W -> listOf(N, S)
            else -> listOf(direc)
        }
        '-' -> when(direc) {
            N, S -> listOf(E, W)
            else -> listOf(direc)
        }
        '/' -> when(direc) {
            N -> listOf(E)
            S -> listOf(W)
            E -> listOf(N)
            W -> listOf(S)
            else -> listOf(direc)
        }
        '\\' -> when(direc) {
            N -> listOf(W)
            S -> listOf(E)
            E -> listOf(S)
            W -> listOf(N)
            else -> listOf(direc)
        }
        else -> listOf(direc)
    }.map { Ray(pos + it, it) }.filter { it.pos in this@Solution }

    private operator fun get(c: Coord): Char = field[c.i][c.j]
    private operator fun contains(c: Coord): Boolean = c.i in field.indices && c.j in field[c.i].indices

    private fun Set<Coord>.energyLayout(): String = field.indices.map { i ->
        field[i].indices.map { j -> if (Coord(i, j) in this) '#' else '.' }.joinToString("")
    }.joinToString("\n")
}

