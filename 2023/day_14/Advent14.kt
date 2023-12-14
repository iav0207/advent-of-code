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

    // Left the original solution for part 1 here just for fun.
    val canRollUpTo = MutableList(m) { 0 }
    var load = 0
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
    check(load == Roller(input).run { tilt(Direction.NORTH); load() })

    Roller(input).apply {
        rotate(1_000_000_000)
        println("Part 2: ${load()}")
    }
}

typealias Field = List<Row>
typealias Row = MutableList<Char>
typealias Snapshot = List<List<Char>>
typealias Load = Int
data class Coord(val i: Int, val j: Int)
operator fun Coord.plus(o: Coord) = Coord(i + o.i, j + o.j)

enum class Direction(val delta: Coord) {
    NORTH(Coord(-1, 0)),
    SOUTH(Coord(1, 0)),
    EAST(Coord(0, 1)),
    WEST(Coord(0, -1)),
}

class Roller(input: List<String>) {
    val field: Field = input.map { it.toMutableList() }

    fun load(): Load = field.withIndex().sumOf { (i, row) -> row.count { it == 'O' }.times(field.size - i) }

    fun rotate(times: Int) {
        val memo = mutableMapOf<Snapshot, Int>(snapshot() to 0)
        for (i in 1..times) {
            cycle()
            val state = snapshot()
            if (state in memo) {
                val repetitionLength = i - memo[state]!!
                val cyclesLeft = times - i
                rotateWithNoMemo(cyclesLeft % repetitionLength)
                break
            }
            memo[state] = i
        }
    }

    private fun snapshot() = field.map { it.toList() }
    private fun rotateWithNoMemo(times: Int) = repeat(times) { cycle(); debugField() }
    private fun debugField() = debug { field.map { it.joinToString("") }.joinToString("\n") + "\n" }

    private fun cycle() = sequenceOf(Direction.NORTH, Direction.WEST, Direction.SOUTH, Direction.EAST).forEach { tilt(it) }

    fun tilt(direc: Direction) = coords(direc)
            .filter { get(it) == 'O' }
            .forEach { roll(it, direc) }

    private fun roll(from: Coord, d: Direction) = generateSequence(from) { it + d.delta }
        .takeWhile { it == from || (it in this && get(it) == '.') }
        .last()
        .also { to -> set(from, '.'); set(to, 'O') }

    // Coordinates sequence that is safe to use when the board is tilted in the given direction.
    private fun coords(direction: Direction): Sequence<Coord> = when(direction) {
        Direction.NORTH -> coords(iReversed = false, jReversed = false)
        Direction.SOUTH -> coords(iReversed = true, jReversed = false)
        Direction.EAST -> coords(iReversed = false, jReversed = true)
        Direction.WEST -> coords(iReversed = false, jReversed = false)
    }

    // Coordinates sequence over all coordinates, where ranges of row or column indices can be reversed.
    private fun coords(iReversed: Boolean, jReversed: Boolean) =
        field.indices
            .run { if (iReversed) reversed() else this }
            .asSequence()
            .flatMap { i ->
                field[0].indices
                    .run { if (jReversed) reversed() else this }
                    .map { j -> Coord(i, j) }
            }

    operator fun contains(c: Coord) = c.i in field.indices && c.j in field[c.i].indices
    operator fun get(c: Coord): Char = field[c.i][c.j]
    operator fun set(c: Coord, ch: Char) = Unit.also { field[c.i][c.j] = ch }
}

