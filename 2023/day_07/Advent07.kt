package advent2023.day07

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val hands = generateSequence { readLine()?.trimEnd() }.toList()
        .map { it.split(" ").run { Row(get(0), get(1).toInt()) } }

    (1..2).forEach { part ->
        val order = Solution(withJokers = part > 1).order
        var totalWinnings = hands.sortedWith(order).debug()
            .withIndex()
            .sumOf { (i, it) -> val rank = i + 1; rank * it.bid }
        println("Part $part: $totalWinnings")
    }
}

data class Row(val hand: Hand, val bid: Int)
typealias Hand = String
typealias Card = Char
typealias Count = Int

class Solution(private val withJokers: Boolean = false) {
    val order: Comparator<Row> get() = compareBy<Row> { it.hand.type }
        .then({ a, b -> compareCards(a.hand, b.hand) })
        .reversed()

    private fun compareCards(a: Hand, b: Hand): Int = a.zip(b)
        .map { (ai, bi) -> cardsByValue.compare(ai, bi) }
        .firstOrNull { it != 0 } ?: 0

    private val cardsByValue: Comparator<Card> = compareBy { cardValuePrecedence.indexOf(it) }
    private val cardValuePrecedence = if (withJokers) "AKQT98765432J" else "AKQJT98765432"

    private val Hand.type: Int get() = typeChecks.indexOfFirst { it() }
    private val typeChecks: List<Hand.() -> Boolean> = listOf(
        { isFiveOfAKind() },
        { isFourOfAKind() },
        { isFullHouse() },
        { isThreeOfAKind() },
        { isTwoPair() },
        { isOnePair() },
        { isHighCard() },
    )

    private fun Hand.isFiveOfAKind() = 5 in counts()
    private fun Hand.isFourOfAKind() = 4 in counts()
    private fun Hand.isFullHouse() = counts() == listOf(2, 3)
    private fun Hand.isThreeOfAKind() = counts() == listOf(1, 1, 3)
    private fun Hand.isTwoPair() = counts() == listOf(1, 2, 2)
    private fun Hand.isOnePair() = counts() == listOf(1, 1, 1, 2)
    private fun Hand.isHighCard() = groups().size == 5

    private fun Hand.counts(): List<Count> = groups().values.sorted()

    private fun Hand.groups(): Map<Card, Count> = groupingBy { it }.eachCount()
        .run { if (withJokers) applyJokers() else this }

    private fun Map<Card, Count>.applyJokers(): Map<Card, Count> = toMutableMap().apply {
        val jokersCount = remove('J') ?: 0
        val strongestCardRemaining = maxByOrNull { (_, count) -> count }?.key ?: 'A'
        strongestCardRemaining.let { put(it, jokersCount.plus(get(it) ?: 0)) }
    }
}

