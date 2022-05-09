package advent2020.day13

import java.math.BigInteger
import kotlin.math.*
import java.util.*

var debug = false
fun debug(offsets: () -> Any): Unit = if (debug) println(offsets()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readLine()?.trimEnd() }.toList()
    debug { input }

    part1(input)
    part2(input).also { check(it == part2chinese(input)) }
}

fun part1(input: List<String>) {
    val earliest = input[0].toLong()
    val busses = input[1].split(",").filterNot { it == "x" }.map { it.toLong() }.toList()

    val (departAt, busId) = generateSequence(earliest) { it + 1 }
        .flatMap { t -> busses.map { bus -> Pair(t, bus) }.asSequence() }
        .find { (t, bus) -> t % bus == 0L } ?: error ("not found :(")
    val needToWait = departAt - earliest
    println("Can take bus # $busId departing at $departAt, need to wait $needToWait")
    println("Part 1: $busId * $needToWait = ${busId * needToWait}")
}

fun part2(input: List<String>): Long {
    val busses: List<Bus> = input[1].split(",")
        .withIndex()
        .filterNot { (_, id) -> id == "x" }
        .map { (i, id) -> Bus(i.toLong(), id.toLong()) }

    var (t, cycle) = busses[0]
    for ((busDelta, busId) in busses.drop(1)) {
        while ((t + busDelta) % busId != 0L) {
            t += cycle
        }
        cycle *= busId
        debug { "t = $t, cycle = $cycle" }
    }

    println("Part 2: t = $t")
    return t
}

data class Bus(val offset: Long, val id: Long) {
    /** Puts offset in range (0 until id) */
    fun normalize() = copy(offset = offset.mod(id).times(-1).plus(id).mod(id))
}

/*
 * For every i:
 * (t + offsets[i]) % bus_ids[i] == 0
 * t % bus_ids[i] == -offsets[i]
 * t % bus_ids[i] == (bus_ids[i] - (offsets[i] % bus_ids[i])) % bus_ids[i] // just 1 cycle up from the prev line
 */
fun part2chinese(input: List<String>): Long {
    val busses = input[1].split(",")
        .withIndex()
        .filterNot { (_, id) -> id == "x" }
        .map { (i, id) -> Bus(i.toLong(), id.toLong()).normalize() }

    val N = busses.fold(1L) { acc, it -> acc * it.id } // the answer must be in (0 until N)

    var t = 0L
    for ((offset, busId) in busses) {
        val Ni = N / busId  // the product of "other" bus IDs, stepping _t_ by this value does not affect
                            // other busses arrive modulo _t_
                            // we're gonna look for offsets multiple of y_i such that t % busId == offset
        val modInv = Ni.modInverse(busId)
        check((modInv*Ni) % busId == 1L)    // by definition of modInverse
        val inc = offset*modInv*Ni          // t-increment to satisfy this bus
        check(inc % busId == offset)        // this checks that the offset is right for this bus
        check(inc % Ni == 0L)               // this checks that all constraints are met
        t += inc
        t %= N
    }

    debug { "Part 2 Chinese: t = $t" }
    return t
}

fun Long.modInverse(other: Long) = asBigint().modInverse(other.asBigint()).toLong()
fun Long.asBigint() = BigInteger.valueOf(this)

