package advent2023.day02

import kotlin.math.*
import kotlin.text.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readLine()?.trimEnd() }.toList()
    val games = input.map { it.split(": ") }
        .map { (gameStr, revealsStr) -> numbersRegex.find(gameStr)!!.value.toInt() to revealsStr.split("; ") }
        .map { (gameId, revealsStrs) -> Game(gameId, revealsStrs.map { it.toReveal() }) }

    val check = Bag(r = 12, g = 13, b = 14)
    val part1 = games.filter { it.isPossibleWith(check) }.sumOf { it.id }
    println("Part 1 : $part1")
    println("Part 2 : ${games.sumOf { it.minimumBag().power() }}")
}

val numbersRegex = "\\d+".toRegex()

fun String.toReveal() = split(", ")
    .associate { it.split(" ").let { words -> words[1] to words[0].toInt() } }
    .run { Reveal(cubes("red"), cubes("green"), cubes("blue")) }

fun Map<String, Int>.cubes(color: String): Int = get(color) ?: 0

data class Reveal(val r: Int, val g: Int, val b: Int)
data class Bag(val r: Int, val g: Int, val b: Int)
data class Game(val id: Int, val reveals: List<Reveal>)

fun Game.isPossibleWith(bag: Bag): Boolean =
    reveals.all { it.r <= bag.r && it.g <= bag.g && it.b <= bag.b }

fun Game.minimumBag(): Bag =
    reveals.fold(Bag(0, 0, 0)) { acc, it -> Bag(
        r = maxOf(acc.r, it.r),
        g = maxOf(acc.g, it.g),
        b = maxOf(acc.b, it.b),
    ) }

fun Bag.power(): Long = r.toLong() * b * g

