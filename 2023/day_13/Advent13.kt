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

    println("Part 1: ${patterns.sumOf { it.debug { it.joinToString("\n") }.reflectionNumber()!! }}")

    patterns
        .map { original ->
            val originalRefNum = original.reflectionNumber()!!
            original.tryFixSmudges()
                .mapNotNull { p ->
                p.reflectionNumbers()
                    ?.firstOrNull { it != originalRefNum }
                    ?.debug { "number $it in pattern:\n${p.joinToString("\n")}" }
            }.firstOrNull() ?: originalRefNum
        }
        .sum()
        .also { println("Part 2: $it") }
}

typealias Pattern = List<String>

fun Pattern.reflectionNumber(): Int? = findReflectingColumns().firstOrNull() ?: findReflectingRows().firstOrNull()?.times(100)
fun Pattern.reflectionNumbers() = findReflectingColumns() + findReflectingRows().map { it * 100 }

fun Pattern.findReflectingRows(): Sequence<Int> = sequence {
    for (i in (0 until size - 1)) {
        generateSequence(0) { it + 1 }
            .map { inc -> (i - inc) to (i + inc + 1) }
            .takeWhile { (lo, hi) -> lo in indices && hi in indices }
            .all { (lo, hi) -> get(lo) == get(hi) }
            .also { if (it) yield(i + 1) }
    }
}

fun Pattern.findReflectingColumns(): Sequence<Int> = sequence {
    var m = get(0).length
    for (j in (0 until m - 1)) {
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
fun Pattern.column(j: Int): String = indices.map { get(it).get(j) }.joinToString("")
fun Pattern.format() = joinToString("\n")

