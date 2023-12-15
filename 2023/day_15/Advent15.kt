package advent2023.day15

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val steps = generateSequence { readlnOrNull()?.trimEnd() }.first().split(",").debug()
    steps.sumOf { hash(it) }.also { println("Part 1: $it") }

    val boxes = MutableList(256) { Box(it) }
    for (s in steps) when {
        '-' in s -> s.substring(0, s.length - 1).let { label -> boxes[hash(label)].remove(label) }
        else -> s.split("=").first().let { label -> boxes[hash(label)].add(s.asLens()) }
    }
    println("Part 2: ${boxes.sumOf { it.focusingPower() }}")
}

fun hash(s: String): Int = s.asSequence().fold(0) { h, it -> h.plus(it.code).times(17).mod(256) }

data class Lens(val label: String, val focalLength: Int)
fun String.asLens() = split("=").let { Lens(it[0], it[1].toInt()) }

class Box(val number: Int) {
    private val lenses = mutableListOf<Lens>()

    fun focusingPower(): Long = lenses.mapIndexed { i, le -> 1L.plus(number).times(1L + i).times(le.focalLength) }.sum()

    fun add(lens: Lens) {
        lenses.withIndex()
            .find { (_, it) -> it.label == lens.label }
            ?.also { (i, _) -> lenses[i] = lens }
            ?: run { lenses.add(lens) }
        debug { "Box $number: $lenses" }
    }

    fun remove(label: String) {
        lenses.removeIf { it.label == label }
        debug { "Box $number: $lenses" }
    }
}

