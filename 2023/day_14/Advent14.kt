package advent2023.day14

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }.toList()
    val n = input.size
    val m = input[0].length

    val canRollUpTo = MutableList(m) { 0 }
    var load = 0L
    for (i in input.indices) {
        for (j in input[i].indices) {
            val c = input[i][j]
            when {
                c == '#' -> canRollUpTo[j] = i + 1
                c == 'O' -> {
                    load += n - canRollUpTo[j]
                    canRollUpTo[j]++
                }
            }
        }
    }
    println("Part 1: $load")
    println("Part 1: ${Roller(input).run { tiltNorth(); load() }}")

    Roller(input).apply {
        runCycles(1_000_000_000)
        println("Part 2: ${load()}")
    }
}

typealias Field = List<Row>
typealias Row = MutableList<Char>
typealias Snapshot = List<List<Char>>
data class Coord(val i: Int, val j: Int)
enum class Direction(delta: Coord) {
    NORTH(Coord(-1, 0)),
    SOUTH(Coord(1, 0)),
    EAST(Coord(1, 0)),
    WEST(Coord(-1, 0)),
}
val NORTH = Direction.NORTH
val SOUTH = Direction.SOUTH
val EAST = Direction.EAST
val WEST = Direction.WEST

operator fun Coord.plus(o: Coord) = Coord(i + o.i, j + o.j)
fun Coord.inv() = Coord(-i, -j)

class Roller(input: List<String>) {
    val field: Field = input.map { it.toMutableList() }
    val n = field.size
    val m = field[0].size
    val memo = mutableMapOf<Snapshot, Int>().apply { put(snapshot(), 0) }

    fun load(): Long = field.withIndex().sumOf { (i, row) -> row.count { it == 'O' }.toLong().times(n - i) }

    fun runCycles(count: Int) {
        for (i in 1..count) {
            cycle()
            val sn = snapshot()
            if (sn in memo) {
                val repetitionLength = i - memo[sn]!!
                val cyclesLeft = count - i
                runWithNoMemo(cyclesLeft % repetitionLength) // could also be memoized (reversed)
                break
            }
            memo[sn] = i
        }
    }

    private fun runWithNoMemo(count: Int) = repeat(count) { cycle() }

    fun cycle() = apply { tiltNorth(); tiltWest(); tiltSouth(); tiltEast() }
    fun snapshot() = field.map { it.toList() }

    fun debugField() = debug { field.map { it.joinToString("") }.joinToString("\n") + "\n" }

    fun tiltNorth() {
        val canRollTo = MutableList(maxOf(n, m)) { 0 }
        for (c in coords(NORTH)) when(get(c)) {
            '#' -> canRollTo[c.j] = c.i + 1
            'O' -> {
                set(c, '.')
                set(c.copy(i = canRollTo[c.j]), 'O')
                canRollTo[c.j]++
            }
        }
    }
    fun tiltSouth() {
        val canRollTo = MutableList(maxOf(n, m)) { n - 1 }
        for (c in coords(SOUTH)) when(get(c)) {
            '#' -> canRollTo[c.j] = c.i - 1
            'O' -> {
                set(c, '.')
                set(c.copy(i = canRollTo[c.j]), 'O')
                canRollTo[c.j]--
            }
        }
    }
    fun tiltWest() {
        val canRollTo = MutableList(maxOf(n, m)) { 0 }
        for (c in coords(WEST)) when(get(c)) {
            '#' -> canRollTo[c.i] = c.j + 1
            'O' -> {
                set(c, '.')
                set(c.copy(j = canRollTo[c.i]), 'O')
                canRollTo[c.i]++
            }
        }
    }
    fun tiltEast() {
        val canRollTo = MutableList(maxOf(n, m)) { m - 1 }
        for (c in coords(EAST)) when(get(c)) {
            '#' -> canRollTo[c.i] = c.j - 1
            'O' -> {
                set(c, '.')
                set(c.copy(j = canRollTo[c.i]), 'O')
                canRollTo[c.i]--
            }
        }
    }
//     fun tilt(direc: Direction) {
//         val start = when (direc) {
//             NORTH -> 0
//             SOUTH -> n
//             EAST -> m
//             WEST -> 0
//         }
//         val indexedBy: Coord.() -> Int = when(direc) {
//             NORTH, SOUTH -> { j }
//             EAST, WEST -> { i }
//         }
//         val canRollTo = MutableList(maxOf(n, m)) { start }
//         for (c in coords(direc)) when(get(c)) {
//             '#' -> canRollTo
//         }
//         coords(direc).forEach { c -> when 
//     }

    private fun coords(direction: Direction): Sequence<Coord> = when(direction) {
        Direction.NORTH -> coords(false, false)
        Direction.SOUTH -> coords(true, false)
        Direction.EAST -> coords(false, true)
        Direction.WEST -> coords(false, false)
    }

    private fun coords(iInv: Boolean, jInv: Boolean) =
        field.indices
            .run { if (iInv) reversed() else this }
            .asSequence()
            .flatMap { i ->
                field[0].indices
                    .run { if (jInv) reversed() else this }
                    .map { j -> Coord(i, j) }
            }

    operator fun get(c: Coord): Char = get(c.i, c.j)
    operator fun get(i: Int, j: Int): Char = field[i][j]
    operator fun set(c: Coord, ch: Char) = set(c.i, c.j, ch)
    operator fun set(i: Int, j: Int, c: Char) = Unit.also { field[i][j] = c }
}

