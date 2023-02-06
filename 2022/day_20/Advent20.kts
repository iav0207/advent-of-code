import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

debug = "-d" in args

// items are not unique

data class Item(val id: Int, val value: Long)

val original: List<Item> = generateSequence { readLine()?.trimEnd()?.toLong() }
    .withIndex()
    .map { (i, it) -> Item(i, it) }
    .toList()

val len = original.size

fun Int.cyclic(cycleLength: Int): Int = toLong().cyclic(cycleLength).toInt()

fun Long.cyclic(cycleLength: Int): Long {
    var c = this
    if (c < 0) c += cycleLength * (-c/cycleLength + 1)
    c %= cycleLength
    return c
}

fun <T> MutableList<T>.swap(i: Int, j: Int) {
    val ai = get(i)
    val aj = get(j)
    set(i, aj)
    set(j, ai)
}

fun MutableList<Item>.move(id: Int, part: Int = 1) {
    val from: Int = withIndex().find { (_, item) -> item.id == id }!!.index
    val value = get(from).value
    if (part == 1) check(value == original[id].value)
    val to = from
        .plus(value.cyclic(len - 1)) // -1 for skipping self
        .plus(if (value < 0) 1 else 0)
        .cyclic(len)
        .toInt()
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

fun List<Item>.calcAnswer(): Long {
    val indexOfZero = withIndex()
        .find { (_, item) -> item.value == 0L }!!
        .index

    return listOf(1000, 2000, 3000)
        .map { indexOfZero + it }
        .map { get(it % len).value }
        .also { debug { "summing $it" } }
        .sum()
}

class PartOne {
    val mix = original.toMutableList()

    fun solve(): Long {
        debug { "Initial:\t${mix.map { it.value }}" }
        for (id in original.indices) {
            mix.move(id)
            debug { "Moved ${original[id].value}:\t${mix.map { it.value }}" }
        }
        return mix.calcAnswer()
    }
}

class PartTwo {
    val decriptionKey = 811589153
    val mixTimes = 10
    val initial: List<Item> = original.map { it.copy(value = it.value * decriptionKey) }

    val mix: MutableList<Item> = initial.toMutableList()

    fun solve(): Long {
        debug { "Initial:\t${mix.map { it.value }}" }
        repeat(mixTimes) { mixOnce() }
        return mix.calcAnswer()
    }

    fun mixOnce() {
        for (id in initial.indices) {
            mix.move(id, part = 2)
            debug { "Moved ${initial[id].value}:\t${mix.map { it.value }}" }
        }
    }
}

println("Part 1: ${PartOne().solve()}")
println("Part 2: ${PartTwo().solve()}")

