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

class Solution(val withJokers: Boolean = false) {
    val order: Comparator<Row> = compareBy<Row> { it.type }
        .then({ a, b -> compare(a.hand, b.hand) })
        .reversed()

    fun compare(le: Hand, ri: Hand): Int {
        return le.zip(ri)
            .map { (ele, eri) -> cardsByValue().compare(ele, eri) }
            .firstOrNull { it != 0 } ?: 0
    }

    fun cardsByValue(): Comparator<Char> = compareBy { cardValuePrecedence.indexOf(it) }
    val cardValuePrecedence = if (withJokers) "AKQT98765432J" else "AKQJT98765432"

    val Row.type: Int get() = typeChecks.indexOfFirst { hand.it() }
    val typeChecks: List<Hand.() -> Boolean> = listOf(
        { isFiveOfAKind() },
        { isFourOfAKind() },
        { isFullHouse() },
        { isThreeOfAKind() },
        { isTwoPair() },
        { isOnePair() },
        { isHighCard() },
    )

    fun Hand.isFiveOfAKind(): Boolean = 5 in counts()
    fun Hand.isFourOfAKind(): Boolean = 4 in counts()
    fun Hand.isFullHouse(): Boolean = counts() == listOf(2, 3)
    fun Hand.isThreeOfAKind(): Boolean = counts() == listOf(1, 1, 3)
    fun Hand.isTwoPair(): Boolean = counts() == listOf(1, 2, 2)
    fun Hand.isOnePair(): Boolean = counts() == listOf(1, 1, 1, 2)
    fun Hand.isHighCard(): Boolean = groups().size == 5

    fun Hand.counts(): List<Count> = groups().values.sorted()

    fun Hand.groups(): Map<Char, Count> = groupingBy { it }.eachCount()
        .run { if (withJokers) applyJokers() else this }

    fun Map<Char, Count>.applyJokers(): Map<Char, Count> = run {
        val groupsExceptJoker = filterKeys { k -> k != 'J' }.toList().sortedByDescending { it.second }
        val jokersCount = get('J') ?: 0
        val strongestGroup = groupsExceptJoker.firstOrNull() ?: 'A' to 0
        val strongestGroupWithJokers = strongestGroup
            .let { (card, count) -> card to count + jokersCount }
            .debug { "group with joker: $it" }

        listOf(strongestGroupWithJokers)
            .plus(groupsExceptJoker.drop(1))
            .toMap()
            .debug { "before: $this\nafter: $it" }
    }
}

