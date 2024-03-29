package advent2019.day10

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val field = generateSequence { readLine()?.trimEnd() }.toList().let { Field(it) }

    val (station, visibleAsteroidsCount) = placeMonitoringStation(field)
    println("Part 1: ${visibleAsteroidsCount}")

    val the200thToVanish = station.vaporizeAsteroidsClockwise()
        .take(200)
        .onEachIndexed { i, it -> debug { "${i+1}. vaporized $it" } }
        .last()
    println("Part 2: ${the200thToVanish.run { 100*x + y }}") 
}

data class Vector(val x: Int, val y: Int)
typealias Coord = Vector

operator fun Vector.plus(other: Vector) = Vector(x + other.x, y + other.y)

class Field(private val m: MutableMap<Coord, Boolean>, val shape: Pair<Int, Int>) {
    constructor(lines: List<String>) : this(
        m = lines.withIndex()
            .flatMap { (y, line) ->
                line.withIndex()
                    .map { (x, char) -> Coord(x, y) to (char == '#') }
            }
            .toMap().toMutableMap(),
        shape = lines[0].length to lines.size
    )

    operator fun get(c: Coord): Boolean = m[c] ?: false
    operator fun contains(c: Coord) =
        c.x >= 0 && c.y >= 0 && c.x < shape.first && c.y < shape.second

    fun destroyAt(c: Coord) = Unit.also { m[c] = false }

    fun asteroids() = coords().filter { this[it] }

    fun coords(): Sequence<Coord> = sequence {
        for (x in 0 until shape.first) {
            for (y in 0 until shape.second) {
                yield(Coord(x, y))
            }
        }
    }
}

fun placeMonitoringStation(field: Field): Pair<MonitoringStation, Int> = field.asteroids()
    .map { MonitoringStation(field, it) }
    .map { it to it.countVisibleAsteroids() }
    .maxBy { it.second }

class MonitoringStation(val field: Field, val position: Coord) {
    fun countVisibleAsteroids(): Int = scan().count()

    fun scan(): Sequence<Coord> {
        val blindSpots = mutableSetOf<Vector>()
        return generateSequence(1) { it + 1 }
            .onEach { debug { "pos=$position, d=$it" } } 
            .map { distance -> square(distance).filter { it.absolute() in field }.toList() }
            .takeWhile { it.isNotEmpty() }
            .flatMap { it }
            .filter { field[it.absolute()] && it !in blindSpots }
            .distinct()
            .onEach { vec ->
                debug { "detected $vec, abs=${vec.absolute()}" }
                val delta = vec.continuationStep
                generateSequence(vec + delta) { it + delta }
                    .takeWhile { it.absolute() in field }
                    .forEach { blindSpots.add(it) }
            }
    }

    fun vaporizeAsteroidsClockwise(): Sequence<Coord> = sequence {
        while (true) {
            val vaporized = scan()
                .sortedBy { it.angleFrom12OClockClockwise() }
                .map { it.absolute() }
                .onEach { field.destroyAt(it) }
                .toList()
            if (vaporized.isEmpty()) break
            yieldAll(vaporized)
        }
    }

    private fun Vector.absolute(): Coord = plus(position)
}

fun square(distance: Int): Sequence<Vector> = sequence {
    for (y in listOf(-distance, distance)) {
        for (x in -distance..distance) {
            yield(Vector(x, y))
            yield(Vector(y, x))
        }
    }
}

/**
 * To calculate angle theta from the x axis counter-clockwise (traditionally) atan2(y, x)
 * is used. To start from the top, we flip the coordinates, and to run clockwise,
 * we negate the result.
 */
private fun Vector.angleFrom12OClockClockwise(): Double = -atan2(x.toDouble(), y.toDouble())

val Vector.continuationStep get(): Vector {
    val scale = gcd(x, y)
    return Vector(x/scale, y/scale)
}

fun gcd(a: Int, b: Int): Int {
    if (a == 0 || b == 0) return abs(a + b)
    val maxAbs = maxOf(abs(a), abs(b))
    val minAbs = minOf(abs(a), abs(b))
    return gcd(minAbs, maxAbs % minAbs)
}

