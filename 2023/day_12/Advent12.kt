package advent2023.day12

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val records: List<Pair<DamagedRecord, Signature>> = generateSequence { readlnOrNull()?.trimEnd() }.toList()
        .map { it.split(" ") }
        .map { DamagedRecord(it[0]) to it[1].split(",").map { n -> n.toInt() } }

    records.asSequence().sumOf { (rec, sig) ->
        rec.arrangements().count { it.sign() == sig }
            .debug { "${rec.code} - $it arrangements" }
    }.also { println("Part 1: $it") }
}

data class DamagedRecord(val code: String)
typealias Signature = List<Int>

fun DamagedRecord.arrangements(): Sequence<String> = arrangements("", 0)

fun DamagedRecord.arrangements(s: String, i: Int): Sequence<String> = when {
    i == code.length -> sequenceOf(s)
    code[i] == '?' -> sequenceOf('.', '#').flatMap { arrangements(s + it, i + 1) }
    else -> arrangements(s + code[i], i + 1)
}

fun String.sign(): Signature = "#+".toRegex().findAll(this).map { it.value.length }.toList().debug { "sign($this) = $it" }

