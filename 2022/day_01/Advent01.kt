package advent2022.day01

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }.toList().debug()
    val elves = mutableListOf<Long>(0L)
    for (line in input) {
        if (line.isEmpty()) {
            elves.add(0L)
            continue
        }
        val last = elves.size - 1
        elves[last] = elves[last] + line.toLong()
    }
    elves.sortDescending()
    println("Part 1: ${elves.first()}")
    println("Part 2: ${elves.take(3).sum()}")
}

