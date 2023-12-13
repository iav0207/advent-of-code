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

    println("Part 1: ${patterns.sumOf { it.reflectionNumber() }}")

    patterns
        .map { it to it.reflectionNumber() }
        .sumOf { (original, originalRefNum) -> original.tryFixSmudges()
            .flatMap { p -> p.reflectionNumbers().map { p to it } }
            .first { (_, num) -> num != originalRefNum }
            .debug { (p, num) -> debug { "number $num in pattern:\n${p.format()}" } }
            .second
        }
        .also { println("Part 2: $it") }
}

typealias Pattern = List<String>

fun Pattern.reflectionNumber(): Int = reflectionNumbers().first()
fun Pattern.reflectionNumbers() = findReflectingColumns() + findReflectingRows().map { it * 100 }

fun Pattern.findReflectingRows(): Sequence<Int> = sequence {
    for (i in (0 until size - 1)) {
        // going from row i both ways checking rows equality
        generateSequence(0) { it + 1 }
            .map { inc -> (i - inc) to (i + inc + 1) }
            .takeWhile { (lo, hi) -> lo in indices && hi in indices }
            .all { (lo, hi) -> row(lo) == row(hi) }
            .also { if (it) yield(i + 1) }
    }
}

fun Pattern.findReflectingColumns(): Sequence<Int> = sequence {
    var m = row(0).length
    for (j in (0 until m - 1)) {
        // going from column j both ways checking columns equality
        generateSequence(0) { it + 1 }
            .map { inc -> (j - inc) to (j + inc + 1) }
            .takeWhile { (lo, hi) -> lo in rowIndices && hi in rowIndices }
            .all { (lo, hi) -> column(lo) == column(hi) }
            .also { if (it) yield(j + 1) }
    }
}

fun Pattern.tryFixSmudges() = allCoordinates().map { (i, j) -> withFlipped(i, j) }

fun Pattern.allCoordinates(): Sequence<Pair<Int, Int>> = indices.asSequence().flatMap { i -> rowIndices.map { j -> i to j } }

fun Pattern.withFlipped(i: Int, j: Int) = mapIndexed { irow, row -> if (i == irow) row.flipAt(j) else row }
fun String.flipAt(i: Int): String = substring(0, i) + get(i).flip() + substring(i + 1)
fun Char.flip() = if (this == '.') '#' else '.'

val Pattern.rowIndices get() = get(0).indices
fun Pattern.row(i: Int): String = get(i)
fun Pattern.column(j: Int): String = indices.map { row(it)[j] }.joinToString("")
fun Pattern.format() = joinToString("\n")

