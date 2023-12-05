package advent2023.day05

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val categories = generateSequence { readLine()?.trimEnd() }
        .joinToString("\n")
        .split("\n\n")
        .map { it.split(":\\s+".toRegex()) }
        .map { it[0] to it[1].trim().split("\n") }
        .toList().debug()

    val seedStr = categories.first().second.first()
    val mapperGroups = categories.drop(1).map { (_, rangesStr) -> rangesStr.map { it.parseMapper() } }

    fun solveFor(seed: List<Range>): Long = mapperGroups.fold(seed) { ranges, mappers ->
        debug { "\n\n---------" }
        debug { "ranges=$ranges" }
        /*
        cut the range by mappers.map { it.from } edges
        to get the list of ranges that either get fully shifted by a mapper
        or remain unmapped.
        */
        val cuts: List<Range> = ranges.flatMap { range -> range.cut(mappers.map { it.from }) }
        debug { "cuts=$cuts" }
        cuts.map { range ->
            mappers.find { it.willMap(range) }
                ?.map(range)
                ?.first()
                ?: range.debug { "not mapped $it" }
        }.merge()
    }.minOf { it.startIn }

    println("Part 1: ${solveFor(seedStr.parseNumbers().map { it.asUnaryRange() })}")
    println("Part 2: ${solveFor(seedStr.parseRanges())}")
}

fun Long.asUnaryRange() = Range(this, this + 1)
fun String.parseNumbers() = split(" ").map { it.toLong() }
fun String.parseRanges(): List<Range> = parseNumbers().chunked(2).map { (startIn, length) -> Range(startIn, startIn + length) }
fun String.parseMapper() = split(" ").map { it.toLong() }.let { Mapper(it[0], it[1], it[2]) }

/** Get the list of non-overlapping ranges which is equivalent to the original. */
fun List<Range>.merge(): List<Range> = mutableListOf<Range>().also { result ->
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

/** Subtract each of the given ranges from the original. */
fun Range.cut(ranges: List<Range>): List<Range> = ranges.fold(listOf(this)) { cuts, range ->
    cuts.flatMap { it.subtract(range) }
    // it is important to not merge at this point to get proper cuts
}

/** Subtract the given range from the receiver. Returns 0..2 ranges. */
fun Range.subtract(r: Range): List<Range> = getOverlapWith(r)
    ?.let { o -> listOf(Range(startIn, o.endEx), Range(o.endEx, endEx)).filter { it.isPositive } }
    ?: listOf(this)

data class Mapper(val from: Range, val to: Range) {
    constructor(toStartIn: Long, fromStartIn: Long, length: Long) :
        this(Range(fromStartIn, fromStartIn + length), Range(toStartIn, toStartIn + length))

    fun map(value: Long) = value.plus(if (value in from) shift else 0)
        .debug { "$this mapped $value to $it" }

    fun willMap(range: Range) = from.overlapsWith(range)

    fun map(range: Range): List<Range> {
        val overlap = range.getOverlapWith(from) ?: Range(0, 0)
        debug { "$from overlaps with $range" }
        return listOf(
            Range(range.startIn, overlap.startIn),
            Range(overlap.startIn + shift, overlap.endEx + shift),
            Range(overlap.endEx, range.endEx),
        ).filter { it.isPositive }.distinct()
            .debug { "$this mapped $range to $it" }
    }
    private val shift = to.startIn - from.startIn

    override fun toString() = "Mapper {$from -> $to}"
}

data class Range(val startIn: Long, val endEx: Long) {
    val isPositive = endEx > startIn

    operator fun contains(value: Long) = value >= startIn && value < endEx

    fun overlapsWith(o: Range) = startIn < o.endEx && endEx > o.startIn
    fun getOverlapWith(o: Range): Range? =
        if (overlapsWith(o)) Range(maxOf(startIn, o.startIn), minOf(endEx, o.endEx)) else null

    override fun toString() = "[$startIn..${endEx - 1}]"
}

