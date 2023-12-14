package advent2023.day14

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }.toList()
    val n = input.size
    val m = input[0].length

    val canRollUpTo = MutableList(m) { 0 }
    var load = 0L
    for (i in input.indices) {
        for (j in input[i].indices) {
            val c = input[i][j]
            when {
                c == '#' -> canRollUpTo[j] = i + 1
                c == 'O' -> {
                    load += n - canRollUpTo[j]
                    canRollUpTo[j]++
                }
            }
        }
    }
    println("Part 1: $load")
}

