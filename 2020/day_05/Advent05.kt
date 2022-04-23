package advent2020.day05

import kotlin.math.*

var debug = false
fun debug(a: Any) = if (debug) println(a) else Unit
fun debug(a: () -> Any) = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args

    val boardingPasses: List<BoardingPass> = generateSequence { readlnOrNull() }.map { BoardingPass(it) }.toList()

    val highest = boardingPasses.maxByOrNull { it.seat.id } ?: error("no boarding passes? o_O")
    println("Max seat ID: $highest")

    val takenSeatIds = boardingPasses.map { it.seat.id }.toSet()

    val mySeat = allSeats()
        .filter { it.row != 0 && it.row != 127 }
        .filter { it.id !in takenSeatIds }
        .find {
            it.id !in takenSeatIds
                && (it.id - 1) in takenSeatIds
                && (it.id + 1) in takenSeatIds
        }

    println("My seat: $mySeat")
}

class Seat(val row: Int, val col: Int) {
    val id: Int = 8*row + col
    override fun toString() = "Seat ID=$id row=$row col=$col"
}

class BoardingPass(val encoded: String) {
    val seat = Seat(findSeat(Search(0, 7, 'B')), findSeat(Search(7, 3, 'R')))
    val row: Int = seat.row
    val col: Int = seat.col

    init { debug { "$this seat ID is ${seat.id}" } }

    override fun toString(): String = "Boarding pass $encoded $seat"
}

fun BoardingPass.findSeat(search: Search): Int {
    debug { search }
    var seat = (2.0.pow(search.steps) - 1) / 2
    (0 until search.steps).forEach { i ->
        val instruction = encoded.get(search.startIdx + i)
        val up = instruction == search.up
        val step = 2.0.pow(search.steps - 2 - i) * up.sign()
        seat += step
        debug { "$i. $instruction $seat" }
    }
    return seat.toInt()
}

fun allSeats(): Sequence<Seat> = sequence {
    for (row in 0..127) {
        for (col in 0..7) {
            yield(Seat(row, col))
        }
    }
}

fun Boolean.sign(): Int = if (this) 1 else -1

data class Search(val startIdx: Int, val steps: Int, val up: Char)

