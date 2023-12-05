package advent2023.day05

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
// fun <T> T.debug(a: T.() -> Any): T = also { if (debug) apply(a)

fun main(vararg args: String) {
    debug = "-d" in args
    val categories = generateSequence { readLine()?.trimEnd() }
        .joinToString("\n")
        .split("\n\n")
        .map { it.split(":\\s+".toRegex()) }
        .map { it[0] to it[1].trim().split("\n") }
        .onEach { debug { it } }

    val seedStr = categories.first().second.first()
    val mapperGroups = categories.drop(1).map { (_, rangesStr) -> rangesStr.map { it.parseMapper() } }

    fun Long.asRange() = Range(this, this + 1)
    val part1 = mapperGroups
        .fold(seedStr.parseNumbers().map { it.asRange() }) { ranges, mappers ->
            debug { "\n\n---------" }
            debug { "ranges=$ranges" }
            val mappersByStart = mappers.associateBy { it.from.startIn }.also { debug { "mappers: $it" } }
            // cut the range by mappers.map { it.from } edges
            val cuts: List<Range> = ranges.flatMap { range -> range.cut(mappers.map { it.from }) }
            debug { "cuts=$cuts" }
            cuts.map { range ->
                mappers.find { it.willMap(range) }
                    ?.map(range)
                    ?.first()
                    ?.also { debug { "mapped $range to $it" } }
                    ?: range.also { debug { "not mapped $it" } }
            }.reduce()
//             acc.map { value ->
//                 mappers.filter { value in it.from }
//                     .map { it.map(value) }
//                     .minOrNull() ?: value
//             }
              //  .also { debug { it } }
        }
        .minOf { it.startIn }
    
    println("Part 1: $part1")

    val part2 = mapperGroups
        .fold(seedStr.parseRanges()) { ranges, mappers ->
            debug { "\n\n---------" }
            debug { "ranges=$ranges" }
            val mappersByStart = mappers.associateBy { it.from.startIn }.also { debug { "mappers: $it" } }
            // cut the range by mappers.map { it.from } edges
            val cuts: List<Range> = ranges.flatMap { range -> range.cut(mappers.map { it.from }) }
            debug { "cuts=$cuts" }
            cuts.map { range ->
                mappers.find { it.willMap(range) }
                    ?.map(range)
                    ?.first()
                    ?.also { debug { "mapped $range to $it" } }
                    ?: range.also { debug { "not mapped $it" } }
            }.reduce()
        }
        .minOf { it.startIn }

    println("Part 2: $part2")
}

fun Range.cut(ranges: List<Range>): List<Range> = ranges.sortedBy { it.startIn }
    .fold(listOf(this)) { acc, it -> acc.flatMap { each -> each.subtract(it) } }

fun Range.subtract(o: Range): List<Range> = getOverlapWith(o)?.let { overlap ->
    listOf(
        Range(startIn, overlap.endEx),
        Range(overlap.endEx, endEx),
    ).filter { it.isPositive }
} ?: listOf(this)

fun String.parseNumbers() = split(" ").map { it.toLong() }
fun String.parseRanges(): List<Range> = parseNumbers().chunked(2).map { (startIn, length) -> Range(startIn, startIn + length) }
fun String.parseMapper() = split(" ").map { it.toLong() }.let { Mapper(it[0], it[1], it[2]) }

fun List<Range>.reduce(doCheck: Boolean = true): List<Range> = mutableListOf<Range>().also { result ->
    val startsIn: Map<Long, Int> = groupingBy { it.startIn }.eachCount()
    val endsEx: Map<Long, Int> = groupingBy { it.endEx }.eachCount()
    var acc = 0
    var prev: Long? = null
    for (point in (startsIn.keys union endsEx.keys).sorted()) {
        if (acc == 0) { prev = point }
        acc += (startsIn[point] ?: 0) - (endsEx[point] ?: 0)
        if (acc == 0) result.add(Range(prev!!, point)).also { prev = null }
        check(acc >=0)
    }
    check(acc == 0 && prev == null)
}

data class Mapper(val from: Range, val to: Range) {
    constructor(toStartIn: Long, fromStartIn: Long, length: Long) :
        this(Range(fromStartIn, fromStartIn + length), Range(toStartIn, toStartIn + length))

    fun map(value: Long) = (value + if (value in from) to.startIn - from.startIn else 0)
        //.also { debug { "$this mapped $value to $it" } }

    fun willMap(range: Range) = from.overlapsWith(range)

    fun map(range: Range): List<Range> {
        val overlap = range.getOverlapWith(from) ?: Range(0, 0)
        debug { "$from overlaps with $range" }
        return listOf(
            Range(range.startIn, overlap.startIn),
            Range(overlap.startIn + shift, overlap.endEx + shift),
            Range(overlap.endEx, range.endEx),
        ).filter { it.isPositive }.distinct()
    }
    val shift = to.startIn - from.startIn
}

data class Range(val startIn: Long, val endEx: Long) {
    val isPositive = endEx > startIn

    operator fun contains(value: Long) = value >= startIn && value < endEx

    fun overlapsWith(o: Range) = startIn < o.endEx && endEx > o.startIn
    fun getOverlapWith(o: Range): Range? =
        if (overlapsWith(o)) Range(maxOf(startIn, o.startIn), minOf(endEx, o.endEx)) else null

    override fun toString() = "[$startIn..${endEx - 1}]"
}

