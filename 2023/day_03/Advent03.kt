package advent2023.day03

import kotlin.math.*
import kotlin.text.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readLine()?.trimEnd() }.toList()

    debug { input }

	fun around(i: Int, jRange: IntRange): Sequence<Pos> = sequence {
		for (ii in maxOf(0, i - 1) .. minOf(input.size - 1, i + 1))
			for (jj in maxOf(0, jRange.start - 1) .. minOf(input[i].length - 1, jRange.endInclusive + 1))
				yield(Pos(ii, jj))
	}
	fun Pos.char() = input[i][j]

	val partNumbers = (0 until input.size)
		.flatMap { i -> "\\d+".toRegex().findAll(input[i]).map { Num(it.value.toInt(), i, it.range) } }
		.filter { around(it.i, it.js).map { it.char() }.any { c -> c != '.' && !c.isDigit() } }

	println("Part 1: ${partNumbers.map { it.value }.sum()}")

	val ratiosSum = partNumbers
		.flatMap { n -> around(n.i, n.js).filter { it.char() == '*' }.map { it to n } }
		.groupBy({ it.first }, { it.second })
		.filterValues { it.size == 2 }
		.mapValues { (_, nums) -> nums[0].value * nums[1].value }
		.values.sum()

	println("Part 2: $ratiosSum")
}

data class Pos(val i: Int, val j: Int)
data class Num(val value: Int, val i: Int, val js: IntRange)

