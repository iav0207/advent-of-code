package advent2023.day08

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }.toList()
    val instructions = input[0]
    val network = input.drop(2).map { "\\w+".toRegex().findAll(it) }
        .map { it.map { match -> match.value }.toList() }
        .associate { it[0] to (it[1] to it[2]) }.debug()

    var curr = "AAA"
    var steps = 0
    for (instruction in instructions.cycle()) {
        if (curr == "ZZZ") break
        curr = network[curr]!!.run { if (instruction == 'L') first else second }
        steps++
    }
    println("Part 1: $steps")
}

fun String.cycle() = generateSequence(0) { it + 1 }.map { get(it % length) }
