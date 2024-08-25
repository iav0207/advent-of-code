package advent2019.day16

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }.first()
        .toCharArray().map { it.digitToInt() }.debug()

    generateSequence(input) { signal ->
        List(signal.size) { pattern(it) }
            .map { pattern -> pattern.zip(signal.asSequence()) { a, b -> a * b }.sum().abs().mod(10) }
    }
        .drop(100)
        .first()
        .debug()
        .also { println("Part 1: ${it.take(8).joinToString("")}") }

    val offset = input.take(7).joinToString("").toInt()
    generateSequence(input.times(10_000).drop(offset).toMutableList()) { fft ->
        (fft.size - 2 downTo 0).forEach { i -> fft[i] = fft[i].plus(fft[i+1]).abs().mod(10) }
        fft
    }
        .drop(100).first()
        .also { println("Part 2: ${it.take(8).joinToString("")}") }
}

fun pattern(repetition: Int): Sequence<Int> = loop
    .flatMap { listOf(it).cycle().take(repetition + 1) }
    .cycle().drop(1)
val loop = listOf(0, 1, 0, -1)

fun <T> List<T>.times(n: Int) = cycle().take(n*size)
fun <T> List<T>.cycle() = asSequence().cycle()
fun <T> Sequence<T>.cycle(): Sequence<T> = generateSequence { this }.flatMap { it }
fun Int.abs() = abs(this)

