package advent2023.day07

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val hands = generateSequence { readLine()?.trimEnd() }.toList()
        .map { it.split(" ").run { Hand(get(0), get(1).toInt()) } }

    (1..2).forEach { part ->
        val order = Solution(withJokers = part > 1).order
        var totalWinnings = hands.sortedWith(order).debug()
            .withIndex()
            .map { (i, it) -> (i + 1) * it.bid }.debug()
            .sum()
        println("Part $part: $totalWinnings")
    }
}

data class Hand(val cards: String, val bid: Int)

class Solution(val withJokers: Boolean = false) {
    val order: Comparator<Hand> = compareBy<Hand> { it.type }
        .then({ a: Hand, b: Hand -> compareCards(a.cards, b.cards) })
        .reversed()

    fun compareCards(le: String, ri: String): Int {
        return le.zip(ri).map { (ele, eri) -> compareCards().compare(ele, eri) }.firstOrNull { it != 0 } ?: 0
    }
    fun compareCards(): Comparator<Char> = compareBy { cardValuePrecedence.indexOf(it) }
    val cardValuePrecedence = if (withJokers) "AKQT98765432J" else "AKQJT98765432"

    val Hand.type: Int get() = typeChecks.indexOfFirst { cards.it() }

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
    fun String.groups(): Map<Char, Int> = groupingBy { it }.eachCount().run {
        if (!withJokers) return this
        val groupsExceptJoker = filterKeys { k -> k != 'J' }.toList().sortedBy { it.second }
        val jokersCount = get('J') ?: 0
        val bestGroup = groupsExceptJoker.firstOrNull() ?: 'A' to 0
        val bestGroupWithJoker = bestGroup.run { first to second + jokersCount }
            .debug { "group with joker: $it" }
        listOf(bestGroupWithJoker)
            .plus(groupsExceptJoker.drop(1))
            .toMap()
            .debug { "before: $this\nafter: $it" }
        // Part 2 passes on the example, but the answer on the real input is not accepted
        // 252634690 is too high
        // 249002740 is too low
        // 250002740 is too high
    }
}

