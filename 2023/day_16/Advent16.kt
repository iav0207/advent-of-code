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
    fun part1(): Int = followTheBeam().map { it.pos }.toSet().debug { it.energy() }.size

    private fun Set<Coord>.energy(): String = field.indices.map { i ->
        field[i].indices.map { j -> if (Coord(i, j) in this) '#' else '.' }.joinToString("")
    }.joinToString("\n")

    private fun followTheBeam(): Set<Ray> {
        val todo = ArrayDeque<Ray>()
        val seen = mutableSetOf<Ray>()
        val start = Ray(Coord(0, 0), E)
        todo.add(start)
        while (todo.isNotEmpty()) {
            val ray = todo.removeFirst()
            debug { "seen $seen" }
            if (!seen.add(ray)) { debug { "seen $ray, exit" }; continue }
            beam(ray)
                .onEach { seen.add(it) }
                .takeWhile { it.pos in this }
                .last().debug { "ended up in $it" }
                .let { continueFrom(it).debug { "will continue in $it" } }
                .forEach { next -> todo.add(next); debug { "todo $next" } }
        }
        return seen
    }

    private fun beam(start: Ray): Sequence<Ray> = sequence {
        val direc = start.direc
        var coord = start.pos
        debug { "beam start $start" }
        while (coord in this@Solution) {
            val elem = get(coord)
            debug { "yield at $coord" }
            yield(Ray(coord, direc))
            if (coord != start.pos && !canProceedIn(direc, elem)) break
            coord = coord + direc
        }
    }

    private fun continueFrom(ray: Ray): List<Ray> = mutableListOf<Ray>().apply {
        fun continueToThe(vararg direcs: Coord) = direcs.forEach { add(Ray(ray.pos, it)) }

        when(get(ray.pos)) {
            '|' -> if (ray.direc.isHorizontal()) continueToThe(N, S)
            '-' -> if (ray.direc.isVertical()) continueToThe(E, W)
            '/' -> when(ray.direc) {
                N -> continueToThe(E)
                S -> continueToThe(W)
                E -> continueToThe(N)
                W -> continueToThe(S)
            }
            '\\' -> when(ray.direc) {
                N -> continueToThe(W)
                S -> continueToThe(E)
                E -> continueToThe(S)
                W -> continueToThe(N)
            }
        }
    }

    private fun canProceedIn(direc: Coord, c: Char): Boolean =
        c == '.' || (direc.isVertical() && c == '|') || (direc.isHorizontal() && c == '-')

    operator fun get(c: Coord): Char = field[c.i][c.j]
    operator fun contains(c: Coord): Boolean = c.i in field.indices && c.j in field[c.i].indices
}

