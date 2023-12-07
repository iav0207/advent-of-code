package advent2023.day07

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val hands = generateSequence { readLine()?.trimEnd() }.toList()
        .map { it.split(" ").run { Hand(get(0), get(1).toInt()) } }

    val totalWinnings = hands.sortedWith(order).debug()
        .withIndex()
        .map { (i, it) -> (i + 1) * it.bid }.debug()
        .sum()
    println("Part 1: $totalWinnings")
}

data class Hand(val cards: String, val bid: Int) {
    val type: Int = typeChecks.indexOfFirst { cards.it() }
}

val order: Comparator<Hand> = compareBy<Hand> { it.type }
    .then({ a: Hand, b: Hand -> compareCards(a.cards, b.cards) })
    .reversed()

fun compareCards(le: String, ri: String): Int {
    return le.zip(ri).map { (ele, eri) -> compareCards().compare(ele, eri) }.firstOrNull { it != 0 } ?: 0
}
fun compareCards(): Comparator<Char> = compareBy { cardValuePrecedence.indexOf(it) }
val cardValuePrecedence = "AKQJT98765432"

val typeChecks: List<String.() -> Boolean> = listOf(
    { isFiveOfAKind() },
    { isFourOfAKind() },
    { isFullHouse() },
    { isThreeOfAKind() },
    { isTwoPair() },
    { isOnePair() },
    { isHighCard() },
)

fun String.isFiveOfAKind(): Boolean = groups().size == 1
fun String.isFourOfAKind(): Boolean = groups().values.any { it == 4 }
fun String.isFullHouse(): Boolean = counts() == listOf(2, 3)
fun String.isThreeOfAKind(): Boolean = counts() == listOf(1, 1, 3)
fun String.isTwoPair(): Boolean = counts() == listOf(1, 2, 2)
fun String.isOnePair(): Boolean = counts() == listOf(1, 1, 1, 2)
fun String.isHighCard(): Boolean = groups().size == 5
fun String.counts(): List<Int> = groups().values.sorted()
fun String.groups(): Map<Char, Int> = groupingBy { it }.eachCount()

