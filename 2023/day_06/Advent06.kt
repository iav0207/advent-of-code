package advent2023.day06

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val records = generateSequence { readLine()?.trimEnd() }.toList().debug()
        .map { it.split("\\s+".toRegex()).drop(1).map { v -> v.toInt() } }
        .let { (times, distances) -> times.indices.map { i -> Run(times[i], distances[i]) } }
        .debug()
    for (t in 0..records.minOf { it.time }) {
        
    }
    records
        .map { r ->
            generateSequence(1) { it + 1 }
                .take(r.time)
                .count { releaseTime ->
                    val timeLeft = r.time - releaseTime
                    val speed = releaseTime
                    val distance = timeLeft * speed
                    distance > r.distance
                }
        }
        .fold(1) { acc, it -> acc * it }
        .also { println("Part 1: $it") }
}

data class Run(val time: Int, val distance: Int)

