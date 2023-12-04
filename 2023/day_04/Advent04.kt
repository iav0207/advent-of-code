package advent2023.day04

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readLine()?.trimEnd() }.toList()
    val cards = listOf(Card(0, emptyList(), emptyList())) + input.map { it.toCard() }
    println("Part 1: ${cards.sumOf { it.points }}")
    println("Part 2: ${cards.total()}")
}

fun String.toCard() = split("|").flatMap { it.split(":") }
    .map { "\\d+".toRegex().findAll(it).toList().map { m -> m.value.toInt() } }
    .let { Card(it[0][0], it[1], it[2]) }

data class Card(val id: Int, val winning: List<Int>, val hand: List<Int>) {
    val matchingNumbers = winning.toSet().let { w -> hand.count { it in w } }

    val points: Int get() = matchingNumbers
        .let { c -> if (c == 0) 0 else 2.pow(c - 1) }
        .also { debug { "Card $id is worth $it points. $this" } }
}

fun Int.pow(n: Int): Int = toDouble().pow(n).toInt()

fun List<Card>.total(): Long = run {
    val counts = MutableList(size) { if (it == 0) 0L else 1L }
    val todo = ArrayDeque<Int>()
    indices.drop(1).forEach { todo.add(it) }

    fun cnt(i: Int): Long = generateSequence(i + 1) { it + 1 }
        .take(get(i).matchingNumbers)
        .onEach { todo.add(it) }
        .count().toLong()

    while (todo.isNotEmpty()) todo.removeFirst().also { counts[it] += cnt(it) }
    counts.sum()
}

