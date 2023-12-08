package advent2023.day08

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: Node) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }.toList()

    val instructions = input[0]
    val network = input.drop(2).map { "\\w+".toRegex().findAll(it) }
        .map { it.map { match -> match.value }.toList() }
        .associate { it[0] to (it[1] to it[2]) }.debug()

    var curr = "AAA"
    var steps = instructions.cycle()
        .onEach { curr = network[curr]!!.apply(it) }
        .takeWhile { curr != "ZZZ" }
        .count()

    println("Part 1: $steps")

    fun stepsToFinishFrom(start: Node): Int {
        val instruction = instructions.cycle().iterator()
        return generateSequence(start) { network[it]!!.apply(instruction.next()) }
            .takeWhile { it[2] != 'Z' }
            .count()
    }

    steps = network.keys.filter { it[2] == 'A' }
        .map { stepsToFinishFrom(it).toLong() }
        .debug()
        .reduce { acc, it -> leastCommonMultiple(acc, it.toLong()) }

    println("Part 2: $steps")
}

fun leastCommonMultiple(a: Long, b: Long): Long = a * b / greatestCommonDivisor(a, b)

fun greatestCommonDivisor(a: Long, b: Long): Long {
    if (a == 0L || b == 0L) return a + b
    val lo = minOf(a, b)
    return greatestCommonDivisor(maxOf(a, b) % lo, lo)
}

typealias Node = String
typealias Instruction = Char

fun Pair<Node, Node>.apply(inst: Instruction) = if (inst == 'L') first else second
fun String.cycle() = generateSequence(0) { it + 1 }.map { get(it % length) }
