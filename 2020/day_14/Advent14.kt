package advent2020.day14

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val lines = generateSequence { readLine()?.trimEnd() }.iterator()

    val mem1 = mutableMapOf<ULong, ULong>()
    val mem2 = mutableMapOf<ULong, ULong>()
    var mask: Mask = lines.next().parseMask()
    while (lines.hasNext()) {
        val line = lines.next()
        if ("mask" in line) {
            mask = line.parseMask()
            continue
        }
        val entry = line.parsePut()
        mem1[entry.first] = mask.applyStrict(entry.second)
        for (addr in mask.applyFloating(entry.first)) {
            mem2[addr] = entry.second
        }
    }
    println("Part 1: ${mem1.values.sum()}")
    println("Part 2: ${mem2.values.sum()}")
}

fun String.parseMask() = Mask(split(" ").last())
fun String.parsePut(): Pair<ULong, ULong> = split(Regex("[\\[\\]]"))[1].toULong() to split(" ").last().toULong()

class Mask(private val str: String) {
    private val ones = str.toULongWithSetBitsWhere { it == '1' }
    private val zeros = str.toULongWithSetBitsWhere { it != '0' }
    private val xBits: List<Int> = str.withIndex()
        .filter { (_, c) -> c == 'X' }
        .map { (i, _) -> 35 - i } // transforming char index (left to right) to bit register (right to left)

    fun applyStrict(n: ULong): ULong = n.or(ones).and(zeros).trunc(36)

    fun applyFloating(n: ULong): Sequence<ULong> = sequence {
        var iter = 0.toULong() // bits of the counter are iterating over binary values of xBits elements
        val limit = twoToThe(xBits.size)
        var curr: ULong
        debug { "applying mask $str n = ${n.bin()} ($n) ones = ${ones.bin()} xBits = $xBits" }
        do {
            curr = ones.or(n)
            for ((i, xi) in xBits.withIndex()) {
                curr = if (iter.isSet(i)) curr.setBit(xi) else curr.unsetBit(xi)
                debug { "iter.isSet(i=$i) = ${if (iter.isSet(i)) 1 else 0} curr = ${curr.bin()}" }
            }
            debug { "iter = $iter curr = ${curr.bin()} yield = ${curr.bin()} dec(yield) = ${curr}" }
            yield(curr)
        } while (++iter < limit)
    }

    override fun toString() = str
}

fun String.toULongWithSetBitsWhere(filter: (Char) -> Boolean): ULong = map { if (filter(it)) '1' else '0' }
    .run { String(toCharArray()).toULong(2) }

fun ULong.bin(): String = toString(2)
fun ULong.trunc(bits: Int) = and(fill(bits))

fun fill(bits: Int): ULong = twoToThe(bits).minus(1.toULong())
fun twoToThe(pow: Int): ULong = 1L.toULong().shl(pow)
fun bitAt(i: Int): ULong = twoToThe(i) // synonym

fun ULong.isSet(i: Int): Boolean = and(bitAt(i)) > 0.toULong()
fun ULong.setBit(i: Int) = or(bitAt(i))
fun ULong.unsetBit(i: Int) = and(bitAt(i).inv())

