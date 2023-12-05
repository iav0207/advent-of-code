package advent2023.day05

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val categories = generateSequence { readLine()?.trimEnd() }
        .joinToString("\n")
        .split("\n\n")
        .map { it.split(":\\s+".toRegex()) }
        .map { it[0] to it[1].trim().split("\n") }
        .onEach { debug { it } }

    var seed: List<Long> = categories.first().second.first().parseNumbers()

    val part1 = categories
        .drop(1)
        .map { (_, rangesStr) -> rangesStr.map { it.parseMapper() } }
        .fold(seed) { acc, mappers ->
            debug { "\n\n---------" }
            acc.map { value ->
                mappers.filter { value in it.from }
                    .map { it.map(value) }
                    .minOrNull() ?: value
            }
                .also { debug { it } }
        }
        .min()
    
    println("Part 1: $part1")

//     val part2 = categories
//         .drop(1)
//         .map { (_, rangesStr) -> rangesStr.map { it.split(" ").map { it.toLong() } } }
//         .fold(categories.first().second.first().split(" ").map { it.toLong() }) { acc, mappers ->
//             debug { "\n\n---------" }
//             acc.map { value ->
//                 mappers.filter { r -> value >= r[1] && value < r[1] + r[2] }
//                     .map { r -> r[0] + (value - r[1]) }
//                     .minOrNull()
//                     ?: value
//             }.also { debug { it } }
//         }
//         .min()
//     
//     println("Part 2: $part2")
}

fun String.parseNumbers() = split(" ").map { it.toLong() }
fun String.parseMapper() = split(" ").map { it.toLong() }.let { Mapper(it[0], it[1], it[2]) }

fun Long.asUnaryRange() = Range(this, this + 1)

data class Mapper(val from: Range, val to: Range) {
    constructor(toStartIn: Long, fromStartIn: Long, length: Long) :
        this(Range(fromStartIn, fromStartIn + length), Range(toStartIn, toStartIn + length))

    fun map(value: Long) = (value + if (value in from) to.startIn - from.startIn else 0)
        .also { debug { "$this mapped $value to $it" } }
}

data class Range(val startIn: Long, val endEx: Long) {
    val isPositive = endEx > startIn

    operator fun contains(value: Long) = value >= startIn && value < endEx

    fun map(mappers: Iterable<Range>): List<Range> = mappers.flatMap { map(it) }.distinct()

    fun map(mapper: Range): List<Range> {
        val overlap = getOverlapWith(mapper) ?: return listOf(this)
        return listOf(
            Range(startIn, overlap.startIn),
            Range(overlap.startIn, overlap.endEx),
            Range(overlap.endEx, endEx),
        ).filter { it.isPositive }.distinct()
    }

    fun overlapsWith(o: Range) = startIn < o.endEx && endEx > o.startIn
    fun getOverlapWith(o: Range): Range? = Range(maxOf(startIn, o.startIn), minOf(endEx, o.endEx))

    override fun toString() = "($startIn .. ${endEx + 1})"
}

