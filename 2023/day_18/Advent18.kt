package advent2023.day18

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }
        .map { it.split("[\\s()]+".toRegex()) }
        .toList()

    var cursor = Coord(0, 0)
    var dugOut = mutableSetOf<Coord>()
    for ((direc, len, _) in input) {
        debug { "$direc $len" }
        cursor = generateSequence(cursor) { it + direcs[direc]!! }
            .take(len.toInt() + 1)
            .onEach { dugOut.add(it) }
            .last()
    }
    fun fillFrom(start: Coord): Sequence<Coord> = sequence {
        val seen = mutableSetOf<Coord>()
        val queue = ArrayDeque<Coord>().also { it.add(start) }
        while (queue.isNotEmpty()) {
            val c = queue.removeFirst()
            if (!seen.add(c)) continue
            yield(c)
            direcs.values.map { c + it }
                .filter { it !in dugOut }
                .forEach { queue.add(it) }
        }
    }
    debug { "direcs: ${input.groupingBy { it.first() }.eachCount()}" }
    val fillStarts = listOf(Coord(1, 1), Coord(-1, -1), Coord(1, -1), Coord(-1, 1))
    val fillers = fillStarts.map { fillFrom(it).iterator() }
    while (fillers.all { it.hasNext() }) {
        fillers.forEach { it.next() }
    }
    fillers.indexOfFirst { !it.hasNext() }
        .let { fillStarts[it] }
        .let { fillFrom(it) }
        .forEach { dugOut.add(it) }
    println("Part 1: ${dugOut.size}")
}

val direcs = mapOf(
    "U" to Coord(0, 1),
    "D" to Coord(0, -1),
    "R" to Coord(1, 0),
    "L" to Coord(-1, 0),
)

data class Coord(val x: Int, val y: Int)
operator fun Coord.plus(o: Coord) = Coord(x + o.x, y + o.y)

