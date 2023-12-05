package advent2023.day05

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val categories = generateSequence { readLine()?.trimEnd() }
        .joinToString("\n")
        .split("\n\n")
        .map { it.split(":\\s+".toRegex()) }
        .map { it[0] to it[1].trim().split("\n") }
        .onEach { debug { it } }

    val part1 = categories
        .drop(1)
        .map { (_, rangesStr) -> rangesStr.map { it.split(" ").map { it.toLong() } } }
        .fold(categories.first().second.first().split(" ").map { it.toLong() }) { acc, ranges ->
            debug { "\n\n---------" }
            debug { ranges }
            acc.map { value ->
                ranges.filter { r -> value >= r[1] && value < r[1] + r[2] }
                    .also { debug { it } }
                    .map { r -> r[0] + (value - r[1]) }
                    .minOrNull()
                    ?: value
            }.also { debug { it } }
        }
        .min()
    
    println("Part 1: $part1")
}

