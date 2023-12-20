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
    debug { "direcs: ${input.groupingBy { it.first() }.eachCount()}" }

    var cursor = Coord(0, 0)
    var dugOut = mutableSetOf<Coord>()
    for ((direc, len, _) in input) {
        debug { "$direc $len" }
        cursor = generateSequence(cursor) { it + direcs[direc]!! }
            .take(len.toInt() + 1)
            .onEach { dugOut.add(it) }
            .last()
    }
    // index all coords of the trench
    // go along the trench
    // you will always have odd numbers of trench polygons to one side of you,
    // and an even number on the other
    // count the distance to the closest trench polygon on the odd side,
    // go to the very end
    // divide the count by four
    // (every inner polygon gets counter from every direction as you traverse the trench)
    val xIdx = dugOut.groupBy { it.x }
    val yIdx = dugOut.groupBy { it.y }
    cursor = Coord(0, 0)
    for ((direc, len, _) in input) {
        for (i in 0 until len.toInt()) {
            cursor += direcs[direc]!!
            debug { "cursor=$cursor" }
            when(direc) {
                "R", "L" -> {
                    val (down, up) = xIdx[cursor.x]!!.filter { it != cursor }.partition { it.y < cursor.y }
                    check(down.size.isOdd() != up.size.isOdd()) { "do=$down up=$up" }
                }
                "U", "D" -> {
                    val (left, right) = yIdx[cursor.y]!!.filter { it != cursor }.partition { it.x < cursor.x }
                    check(left.size.isOdd() != right.size.isOdd()) { "le=$left ri=$right" }
                }
            }
        }
    }
    println("Part 1: ${dugOut.size}")
}

val direcs = mapOf(
    "U" to Coord(0, 1),
    "D" to Coord(0, -1),
    "R" to Coord(1, 0),
    "L" to Coord(-1, 0),
)

val ortho = mapOf(
    "U" to listOf("R", "L"),
    "D" to listOf("R", "L"),
    "R" to listOf("U", "D"),
    "L" to listOf("U", "D"),
)

data class Coord(val x: Int, val y: Int)
operator fun Coord.plus(o: Coord) = Coord(x + o.x, y + o.y)

fun Int.isOdd() = mod(2) == 0

