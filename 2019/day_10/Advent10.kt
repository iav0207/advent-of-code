package advent2019.day10

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readLine()?.trimEnd() }.toList()
    println("Part 1 : ${Solver(Field(input)).solvePartOne()}")
}

data class Vector(val x: Int, val y: Int)
typealias Coord = Vector

operator fun Vector.plus(other: Vector) = Vector(x + other.x, y + other.y)

class Field(private val m: Map<Coord, Boolean>, val shape: Pair<Int, Int>) {
    constructor(lines: List<String>) : this(
        m = lines.withIndex()
            .flatMap { (y, line) ->
                line.withIndex()
                    .map { (x, char) -> Coord(x, y) to (char == '#') }
            }
            .toMap(),
        shape = lines[0].length to lines.size
    )

    operator fun get(c: Coord): Boolean = m[c] ?: false
    operator fun contains(c: Coord) =
        c.x >= 0 && c.y >= 0 && c.x < shape.first && c.y < shape.second

    fun asteroids() = coords().filter { this[it] }

    fun coords(): Sequence<Coord> = sequence {
        for (x in 0 until shape.first) {
            for (y in 0 until shape.second) {
                yield(Coord(x, y))
            }
        }
    }
}

class Solver(private val field: Field) {
    fun solvePartOne(): Int = field.asteroids().maxOf { countVisibleAsteroidsFrom(it) }

    fun countVisibleAsteroidsFrom(c: Coord): Int {
        val detected = mutableSetOf<Vector>()
        val blindSpots = mutableSetOf<Vector>()
        fun Vector.absolute(): Coord = c + this
        generateSequence(1) { it + 1 }
            .onEach { debug { "c=$c, d=$it" } } 
            .map { distance -> square(distance).filter { it.absolute() in field }.toList() }
            .takeWhile { it.isNotEmpty() }
            .flatMap { it }
            .filter { field[it.absolute()] && it !in blindSpots }
            .forEach { vec ->
                debug { "detected $vec, abs=${vec.absolute()}" }
                detected.add(vec)
                val delta = vec.continuationStep
                generateSequence(vec + delta) { it + delta }
                    .takeWhile { it.absolute() in field }
                    .forEach { blindSpots.add(it) }
            }
        return detected.size
    }
}

fun square(distance: Int): Sequence<Vector> = sequence {
    for (y in listOf(-distance, distance)) {
        for (x in -distance..distance) {
            yield(Vector(x, y))
            yield(Vector(y, x))
        }
    }
}

val Vector.continuationStep get() = Vector(
    x = x / gcd(abs(x), abs(y)),
    y = y / gcd(abs(x), abs(y)),
)

fun gcd(a: Int, b: Int): Int {
    if (a == 0 || b == 0) return abs(a + b)
    val maxAbs = maxOf(abs(a), abs(b))
    val minAbs = minOf(abs(a), abs(b))
    return gcd(minAbs, maxAbs % minAbs)
}

