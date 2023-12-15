package advent2023.day15

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val steps = generateSequence { readlnOrNull()?.trimEnd() }.first().split(",").debug()
    steps.sumOf { hash(it) }.also { println("Part 1: $it") }
}

fun hash(s: String): Int = s.asSequence().fold(0) { h, it -> h.plus(it.code).times(17).mod(256) }

