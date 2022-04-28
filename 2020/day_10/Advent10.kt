package advent2020.day10

var debug = false
fun debug(a: Any) = if (debug) println(a) else Unit
fun debug(a: () -> Any) = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args

    val input = generateSequence(0) { readLine()?.trimEnd()?.toInt() }
        .sorted()
        .toList()
        .run { plus(last() + 3) }

    val result = input
        .zipWithNext { prv, nxt -> nxt - prv }
        .groupBy({ it }, { 1 })
        .mapValues { (_, ones) -> ones.size }
        .also { debug { it } }
        .run { (get(1) ?: 0) * (get(3) ?: 0) }

    println("Part 1: $result")

    val arr = MutableList(input.last() + 1) { 0L }

    arr[0] = 1L
    for (i in input.drop(1)) {
        var ai = 0L
        for (j in 1..3) if (i-j >= 0) ai += arr[i-j]
        debug { "arr[$i] = $ai" }
        arr[i] = ai
    }

    println("Part 2: ${arr.last()}")
}

