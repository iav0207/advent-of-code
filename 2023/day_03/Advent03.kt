package advent2023.day03

import kotlin.math.*
import kotlin.text.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readLine()?.trimEnd() }.toList()

    debug { input }
	val partNumbers = mutableListOf<Int>()
	val numbersRegex = "\\d+".toRegex()
	input.withIndex().forEach { (i, line) ->
		val numbers = numbersRegex.findAll(line).map { it.range to it.value.toInt() }
		numbers.filter { (rng, _) ->
			(maxOf(0, i - 1) .. minOf(input.size - 1, i + 1)).any { ii ->
				(maxOf(0, rng.start - 1) .. minOf(line.length - 1, rng.endInclusive + 1)).any { jj ->
					input[ii][jj].let { it != '.' && !it.isDigit() }
				}
			}
		}.forEach { (_, num) -> partNumbers.add(num) }
	}
	println("Part 1: ${partNumbers.sum()}")
}

