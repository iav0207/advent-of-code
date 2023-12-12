package advent2023.day12

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val records: List<Pair<Record, Signature>> = generateSequence { readlnOrNull()?.trimEnd() }.toList()
        .map { it.split(" ") }
        .map { it[0] to it[1].split(",").map { n -> n.toInt() } }

    records.asSequence().sumOf { (rec, sig) ->
        arrangements(rec, sig)
            .debug { "${rec} :: $sig - $it arrangements" }
    }.also { println("Part 1: $it") }
}

typealias Record = String
typealias Signature = List<Int>
data class Group(val c: Char, val n: Int)

fun arrangements(record: String, sign: Signature): Long = when {
    sign.isEmpty() -> if ('#' in record) 0L else 1L
    record.isEmpty() -> 0L
    else -> 0L
        .plusIf(record.canStartWithDot()) { arrangements(record.drop(1), sign) }
        .plusIf(record.canStartWithHashes(sign.first())) { arrangements(record.drop(sign.first() + 1), sign.drop(1)) }
}

fun Long.plusIf(condition: Boolean, increment: () -> Long): Long = plus(if (condition) increment() else 0L)

fun String.canStartWithDot() = first() in ".?"

fun String.canStartWithHashes(count: Int): Boolean = take(count).none { it == '.' }
    && drop(count - 1).isNotEmpty()
    && drop(count).firstOrNull()?.let { it != '#' } ?: true

fun String.sign(): Signature = "#+".toRegex().findAll(this)
    .map { it.value.length }
    .toList()
    .debug { "sign($this) = $it" }

