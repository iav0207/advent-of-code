package advent2023.day13

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val patterns = generateSequence { readlnOrNull()?.trimEnd() }
        .joinToString("\n")
        .split("\n\n")
        .map { it.split("\n") }

    println("Part 1: ${patterns.sumOf { it.debug { it.joinToString("\n") }.reflectionNumber() }}")
}

typealias Pattern = List<String>

fun Pattern.reflectionNumber(): Int = findReflectingColumn() ?: findReflectingRow()?.times(100) ?: 0

fun Pattern.findReflectingRow(): Int? {
    for (i in (0 until size - 1)) {
        generateSequence(0) { it + 1 }
            .map { inc -> (i - inc) to (i + inc + 1) }
            .takeWhile { (lo, hi) -> lo in indices && hi in indices }
            .all { (lo, hi) -> get(lo) == get(hi) }
            .also { if (it) return i + 1 }
    }
    return null
}

fun Pattern.findReflectingColumn(): Int? {
    var m = get(0).length
    val rowIndices = get(0).indices
    for (j in (0 until m - 1)) {
        generateSequence(0) { it + 1 }
            .map { inc -> (j - inc) to (j + inc + 1) }
            .takeWhile { (lo, hi) -> lo in rowIndices && hi in rowIndices }
            .all { (lo, hi) -> column(lo) == column(hi) }
            .also { if (it) return j + 1 }
    }
    return null
}

fun Pattern.column(j: Int): String = indices.map { get(it).get(j) }.joinToString("")

