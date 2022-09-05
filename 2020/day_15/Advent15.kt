package advent2020.day15

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val inputLine = readLine()?.trimEnd() ?: ""
    val input = inputLine.split(",").map { it.toLong() }.toList()

    debug { input }

    val lastTurnsForNumber = mutableMapOf<Long, LongArray>()
    var lastSpokenNumber = 0L

    fun say(num: Long, atTurn: Long) {
        lastTurnsForNumber.compute(num) { _, v ->
            val arr = v ?: LongArray(2) { 0L }
            arr[1] = arr[0]
            arr[0] = atTurn
            arr
        }
        lastSpokenNumber = num
        debug { "${atTurn}. said $num" }
    }

    fun dist(num: Long) = lastTurnsForNumber[num]
        ?.takeUnless { it[1] == 0L }
        ?.let { it[0] - it[1] }
        ?: 0L

    for (t in 1L..30000000L) {
        if (t <= input.size) {
            say(input[t.toInt() - 1].toLong(), t)
            continue
        }
        say(dist(lastSpokenNumber), t)
        if (t == 2020L) println(lastSpokenNumber)
    }
    println(lastSpokenNumber)
}

