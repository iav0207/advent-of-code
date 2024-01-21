package advent2023.day12v2

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args

    check(onHash("#", listOf(1)) == 1L)
    check(onHash("##", listOf(1)) == 0L)
    check(onHash("##?", listOf(3)) == 1L)

    val input = generateSequence { readlnOrNull()?.trimEnd() }.toList()

    input
        .map { it.split(' ') }
        .sumOf { (first, second) -> arrangements(first, second.split(',').map { it.toInt() }) }
        .also { println("Part 1: $it") }

    input
        .map { it.split(' ') }
        .sumOf { (first, second) ->
            val layout = List(5) { first }.joinToString("?")
            val signature = second.split(',').map { it.toInt() }.let { s -> List(5) { s } }.flatten()
            arrangements(layout, signature)
        }
        .also { println("Part 2: $it") }
}

fun arrangements(layout: String, signature: List<Int>): Long = onDot(layout, signature).debug { "$layout $signature = $it" }

val cache = mutableMapOf<Pair<String, List<Int>>, Long>()

fun onDot(layout: String, signature: List<Int>): Long = cache.getOrPut(layout to signature) {
    when {
        signature.isEmpty() && '#' !in layout -> 1
        signature.isEmpty() -> 0
        layout.isEmpty() && signature.isNotEmpty() -> 0
        layout.startsWith('.') -> onDot(layout.drop(1), signature)
        layout.startsWith('#') -> onHash(layout, signature)
        else -> onDot(layout.drop(1), signature) + onHash(layout, signature)
    }.debug { "onDot($layout, $signature) = $it" }
}

fun onHash(layout: String, signature: List<Int>): Long = cache.getOrPut(layout to signature) {
    when {
        signature.isEmpty() && '#' !in layout -> 1
        signature.isEmpty() -> 0
        layout.length < signature.first() -> 0
        layout.take(signature.first()).any { it == '.' } -> 0
        layout.length > signature.first() && layout[signature.first()] == '#' -> 0
        else -> onDot(layout.drop(signature.first() + 1), signature.drop(1))
    }.debug { "onHash($layout, $signature) = $it" }
}
