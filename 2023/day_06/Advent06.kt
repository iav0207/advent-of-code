package advent2023.day06

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readLine()?.trimEnd() }.toList().debug()

    println("Part 1: ${input.parseRecords().productOf { waysToOutrun() }}")
    println("Part 2: ${input.parseRecord().waysToOutrun()}")
}

fun List<String>.parseRecords(): List<Run> =
    map { it.split("\\s+".toRegex()).drop(1).map { v -> v.toLong() } }
        .let { (times, distances) -> times.indices.map { i -> Run(times[i], distances[i]) } }

fun List<String>.parseRecord(): Run =
    map { "\\d+".toRegex().findAll(it).map { it.value }.joinToString("").toLong() }
        .let { (time, dist) -> Run(time, dist) }

fun <T: Any> Iterable<T>.productOf(selector: T.() -> Int) = fold(1) { acc, it -> acc * it.selector() }

data class Run(val time: Long, val distance: Long)

fun Run.waysToOutrun(): Int = generateSequence(1L) { it + 1 }
    .take(time.toInt())
    .count { releaseTime ->
        val timeLeft = time - releaseTime
        val speed = releaseTime
        val distance = timeLeft * speed
        distance > this.distance
    }

