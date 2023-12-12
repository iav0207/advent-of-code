package advent2023.day12

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val records: List<Record> = generateSequence { readlnOrNull()?.trimEnd() }.toList()
        .map { it.split(" ") }
        .map { Record(it[0], it[1].split(",").map { n -> n.toInt() }) }

    records
        .sumOf { it.arrangements().debug { a -> "$it - $a arrangements" } }
        .also { println("Part 1: $it") }

    records
        .map { it.times(5) }
        .sumOf { it.arrangements() }
        .also { println("Part 2: $it") }
}

typealias Spring = String
typealias Signature = List<Int>

data class Record(val spring: Spring, val sign: Signature) {

    fun arrangements(): Long = when {
        sign.isEmpty() -> if ('#' in spring) 0L else 1L
        spring.isEmpty() -> 0L
        else -> cache.getOrPut(this) {
            0L
                .plusIf(spring.canStartWithDot()) { consumeOneDot().arrangements() }
                .plusIf(spring.canStartWithHashes(sign.first())) { consumeHashes().arrangements() }
        }
    }

    fun consumeOneDot() = Record(spring.drop(1), sign)
    fun consumeHashes() = Record(spring.drop(sign.first() + 1), sign.drop(1))

    fun times(n: Int) = Record(
        spring = List(n) { spring }.joinToString("?"),
        sign = generateSequence { sign }.take(n).flatMap { it }.toList(),
    )
}

val cache = mutableMapOf<Record, Long>()

fun Long.plusIf(condition: Boolean, increment: () -> Long): Long = plus(if (condition) increment() else 0L)

fun Spring.canStartWithDot() = first() in ".?"

fun Spring.canStartWithHashes(count: Int): Boolean = take(count).none { it == '.' }
    && drop(count - 1).isNotEmpty()
    && drop(count).firstOrNull()?.let { it != '#' } ?: true

fun Spring.sign(): Signature = "#+".toRegex().findAll(this)
    .map { it.value.length }
    .toList()
    .debug { "sign($this) = $it" }

