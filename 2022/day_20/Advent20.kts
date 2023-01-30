import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

debug = "-d" in args

// items are not unique

data class Item(val id: Int, val value: Long)

val initialArrangement: List<Item> = generateSequence { readLine()?.trimEnd()?.toLong() }
    .withIndex()
    .map { (i, it) -> Item(i, it) }
    .toList()

val len = initialArrangement.size
// TODO better names for cycle and cycLen
val cycle = len - 1 // that's because we carry the item over len-1 remaining positions

val mix = initialArrangement.toMutableList()

fun cyclic(n: Int, cycLen: Int = cycle): Int {
    var c = n
    while (c < 0) c += cycLen
    c %= cycLen
    check(c >= 0)
    return c
}

fun <T> MutableList<T>.swap(i: Int, j: Int) {
    val ai = get(i)
    val aj = get(j)
    set(i, aj)
    set(j, ai)
}

fun MutableList<Item>.move(id: Int) {
    val from: Int = withIndex().find { (_, item) -> item.id == id }!!.index
    val value = get(from).value
    check(value == initialArrangement[id].value)
    val to = cyclic(from + value.toInt(), cycLen = len)
    debug { "Moving $value from $from to $to" }
    if (to == from) return
    val direc = if (value >= 0) 1 else -1
    var (prev, curr) = Pair(from, cyclic(from + direc, cycLen = len))
    debug { "direc $direc" }
    while (true) {
        debug { "swap $prev <> $curr" }
        check(prev != curr)
        swap(prev, curr)
        if (curr == to) break
        prev = curr
        curr = cyclic(curr + direc, cycLen = len)
    }
}

val expected = listOf(
    listOf(1, 2, -3, 3, -2, 0, 4),
    listOf(2, 1, -3, 3, -2, 0, 4),
    listOf(1, -3, 2, 3, -2, 0, 4),
    listOf(1, 2, 3, -2, -3, 0, 4),
    listOf(1, 2, -2, -3, 0, 3, 4),
    listOf(1, 2, -3, 0, 3, 4, -2),
    listOf(1, 2, -3, 0, 3, 4, -2),
    listOf(1, 2, -3, 4, 0, 3, -2),
).map { sublist -> sublist.map { it -> it.toLong() } }

debug { "Initial:\t${initialArrangement.map { it.value }}" }
for (id in initialArrangement.indices) {
    println(id)
    // val actual: List<Long> = mix.map { it.value }
    // val shExpected = expected[id].toMutableList()
    // while (shExpected[0] != actual[0]) shExpected.add(shExpected.removeAt(0))
    // check(shExpected == actual) { "\nid: $id\nExpected:\t${expected[id]}\nActual:\t\t$actual" }
    mix.move(id)
    debug { "Moved ${initialArrangement[id].value}:\t${mix.map { it.value }}" }
}

val indexOfZero = mix.withIndex()
    .find { (_, item) -> item.value == 0L }!!
    .index

val p1 = listOf(1000, 2000, 3000)
    .map { indexOfZero + it }
    .map { mix[it % len].value }
    .also { debug { "summing $it" } }
    .sum()

println("Part 1: $p1")

