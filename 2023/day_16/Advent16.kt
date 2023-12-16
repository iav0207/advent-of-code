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
    }
}

typealias Field = List<String>
data class Coord(val i: Int, val j: Int) {
    override fun toString() = "($i, $j)"
}
operator fun Coord.plus(o: Coord) = Coord(i + o.i, j + o.j)
val N = Coord(-1, 0)
val S = Coord(1, 0)
val E = Coord(0, 1)
val W = Coord(0, -1)
fun Coord.isVertical() = this == N || this == S
fun Coord.isHorizontal() = this == E || this == W

data class Ray(val pos: Coord, val direc: Coord) {
    override fun toString() = "$pos towards $direc"
}

class Solution(private val field: Field) {
    fun part1(): Int = energizedTiles().debug { it.energy() }.size

    private fun Set<Coord>.energy(): String = field.indices.map { i ->
        field[i].indices.map { j -> if (Coord(i, j) in this) '#' else '.' }.joinToString("")
    }.joinToString("\n")

    private fun energizedTiles(): Set<Coord> {
        val energized = mutableSetOf<Coord>()
        val visited = mutableSetOf<Ray>()
        val queue = ArrayDeque<Ray>()
        val start = Ray(Coord(0, 0), E)
        queue.add(start)

        while (queue.isNotEmpty()) {
            val ray = queue.removeFirst()
            if (!visited.add(ray)) continue

            energized.add(ray.pos)
            ray.children().forEach { queue.add(it) }
        }

        return energized
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

    operator fun get(c: Coord): Char = field[c.i][c.j]
    operator fun contains(c: Coord): Boolean = c.i in field.indices && c.j in field[c.i].indices
}

