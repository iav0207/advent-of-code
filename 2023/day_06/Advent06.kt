package advent2023.day06

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readLine()?.trimEnd() }.toList().debug()

    val records = input
        .map { it.split("\\s+".toRegex()).drop(1).map { v -> v.toLong() } }
        .let { (times, distances) -> times.indices.map { i -> Run(times[i], distances[i]) } }
        .debug()

    records
        .map { r ->
            generateSequence(1) { it + 1 }
                .take(r.time.toInt())
                .count { releaseTime ->
                    val timeLeft = r.time - releaseTime
                    val speed = releaseTime
                    val distance = timeLeft * speed
                    distance > r.distance
                }
        }
        .fold(1) { acc, it -> acc * it }
        .also { println("Part 1: $it") }

    val record = input.map { "\\d+".toRegex().findAll(it).map { it.value }.joinToString("").toLong() }
        .let { (time, dist) -> Run(time, dist) }

    generateSequence(1L) { it + 1 }
        .take(record.time.toInt())
        .count { releaseTime ->
            val timeLeft = record.time - releaseTime
            val speed = releaseTime
            val distance = timeLeft * speed
            distance > record.distance
        }.also { println("Part 2: $it") }
}

data class Run(val time: Long, val distance: Long)

