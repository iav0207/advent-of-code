import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

debug = "-d" in args

val part = if ("2" in args) 2 else 1

data class Monkey(
    val id: Int,
    val items: MutableList<Long>,
    val divisor: Long,
    val nextIfTrue: Int,
    val nextIfFalse: Int,
    val operation: List<String>,
) {
    var counter = 0L

    fun makeTurn(worryLimit: Long): List<Throw> = items
        .onEach { counter++ }
        .map { applyOperation(it) }
        .map { if (part == 2) it % worryLimit else it / 3 }
        .map {
            val recipient = if (it % divisor == 0L) nextIfTrue else nextIfFalse
            Throw(recipient, it)
        }
        .also { items.clear() }

    fun applyOperation(item: Long): Long {
        val operator = operation[1]
        val operand = if (operation[2] == "old") item else operation[2].toLong()
        return when (operator) {
            "*" -> item * operand
            "+" -> item + operand
            "-" -> item - operand
            else -> error("unknown operator $operator")
        }
    }
}

data class Throw(val recipient: Int, val item: Long)

fun String.findLongs() = Regex("\\d+").findAll(this).map { it.value.toLong() }.toList()
fun String.findLong() = findLongs().first()
fun String.findInt() = findLong().toInt()

fun List<String>.asMonkey() = Monkey(
    id = get(0).findInt(),
    items = get(1).findLongs().toMutableList(),
    divisor = get(3).findLong(),
    nextIfTrue = get(4).findInt(),
    nextIfFalse = get(5).findInt(),
    operation = get(2).split(" ").takeLast(3),
)

val monkeys = generateSequence { readLine()?.trimEnd() }
    .filter { !it.isBlank() }
    .toList()
    .chunked(6)
    .map { it.asMonkey() }

debug { monkeys }

/*
  The idea of worryLimit in part 2 is that we don't need to know the exact item values,
  we only need to route items to correct monkeys and count item inspections
  by each monkey.
  The only criterion for proper routing is integer division by a number,
  one per monkey.
  Therefore, any value can be taken modulo the product of all monkey divisors
  to limit monkey worry levels.
*/

val worryLimit = monkeys.map { it.divisor }.fold(1L) { acc, it -> acc * it }

debug { "Worry limit is $worryLimit" }

fun makeRound() {
    for (monkey in monkeys) {
        val throws = monkey.makeTurn(worryLimit)
        throws.forEach {
            monkeys[it.recipient].items.add(it.item)
        }
    }
}

repeat(if (part == 2) 10000 else 20) { makeRound() }

debug { monkeys }

val busy = monkeys.map { it.counter }.sorted().takeLast(2)
println("Part $part: Monkey business level is ${busy[0] * busy[1]}")

