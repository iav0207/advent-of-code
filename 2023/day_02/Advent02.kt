package advent2023.day02

import kotlin.math.*
import kotlin.text.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readLine()?.trimEnd() }.toList()
    val games = input.map { it.split(": ") }
        .map { (gameStr, revealsStr) -> gameStr.findNumber() to revealsStr.split("; ") }
        .map { (gameId, revealsStrs) -> Game(gameId, revealsStrs.map { it.toReveal() }) }

    val bag = Bag(r = 12, g = 13, b = 14)
    println("Part 1 : ${games.filter { it.isPossibleWith(bag) }.sumOf { it.id }}")
    println("Part 2 : ${games.sumOf { it.minimumBag().power() }}")
}

fun String.findNumber(): Int = numbersRegex.find(this)!!.value.toInt()
val numbersRegex = "\\d+".toRegex()

fun String.toReveal() = split(", ")
    .associate { it.split(" ").let { (qty, color) -> color to qty.toInt() } }
    .run { Reveal(cubes("red"), cubes("green"), cubes("blue")) }

fun Map<String, Int>.cubes(color: String): Int = get(color) ?: 0

data class CubeSet(val r: Int, val g: Int, val b: Int)
data class Game(val id: Int, val reveals: List<Reveal>)
typealias Bag = CubeSet
typealias Reveal = CubeSet

fun Game.isPossibleWith(bag: Bag): Boolean =
    reveals.all { it.r <= bag.r && it.g <= bag.g && it.b <= bag.b }

fun Game.minimumBag(): Bag = reveals.fold(Bag(0, 0, 0)) { acc, it ->
    Bag(r = maxOf(acc.r, it.r), g = maxOf(acc.g, it.g), b = maxOf(acc.b, it.b))
}

fun Bag.power(): Long = r.toLong() * b * g

