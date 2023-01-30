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

val mix = initialArrangement.toMutableList()

fun Int.cyclic(cycleLength: Int): Int {
    var c = this
    while (c < 0) c += cycleLength
    c %= cycleLength
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
    val to = from
        .plus(value.toInt().cyclic(len - 1)) // -1 for skipping self
        .plus(if (value < 0) 1 else 0)
        .cyclic(len)
    debug { "Moving $value from $from to $to" }
    if (to == from) return
    val direc = if (value >= 0) 1 else -1
    var prev = from
    var curr = from.plus(direc).cyclic(len)
    while (true) {
        debug { "swap $prev <> $curr" }
        swap(prev, curr)
        if (curr == to) break
        prev = curr
        curr = curr.plus(direc).cyclic(len)
    }
}

debug { "Initial:\t${initialArrangement.map { it.value }}" }
for (id in initialArrangement.indices) {
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

