import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

debug = "-d" in args
val initArrangement: List<Long> = generateSequence { readLine()?.trimEnd()?.toLong() }.toList()
val len = initArrangement.size

debug { initArrangement }

val mix = initArrangement.toMutableList()

fun MutableList<Long>.swap(i: Int, j: Int) {
    val ai = get(i)
    val aj = get(j)
    set(i, aj)
    set(j, ai)
}

fun cycle(n: Int): Int {
    var c = n
    while (c < 0) c += len
    return c % len
}

fun MutableList<Long>.move(num: Long) {
    debug { "Move $num" }
    val from = indexOf(num) // FIXME values are not unique :(
    val direc = if (num >= 0L) 1 else -1
    var i = from
    var toGo = Math.abs(num.toInt())
    while (toGo > 0) {
        val j = cycle(i + direc)
        swap(cycle(i), j)
        i += direc
        toGo--
    }
}

initArrangement.asSequence().forEach { mix.move(it); debug { mix } }

val indexOfZero = mix.indexOf(0)
val part1answer = listOf(1000, 2000, 3000)
    .map { mix[(indexOfZero + it) % len] }
    .also { debug { it } }
    .sum()
println("Part 1: $part1answer")
// -2274 is not the right answer
// that's cuz input values are not unique!!

