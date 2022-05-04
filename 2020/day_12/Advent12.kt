package advent2020.day12

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val instructions = generateSequence { readLine()?.trimEnd()?.parse() }.toList()

    val ship1 = Ship1()
    instructions.forEach { ship1.apply(it) }
    println("Part 1: Manhattan distance of the ship is ${ship1.coord.manhattan()}")

    val ship2 = Ship2()
    instructions.forEach { ship2.apply(it) }
    println("Part 2: Manhattan distance of the ship is ${ship2.coord.manhattan()}")
}

fun String.parse() = Instruction(get(0), substring(1, length).toInt())

data class Instruction(val action: Action, val value: Value)

typealias Action = Char
typealias Value = Int
typealias Distance = Value
typealias Angle = Value

class Ship1 {
    var coord: Coord = Coord(0, 0)
    var direction: Direction = Direction.E

    fun apply(instruction: Instruction) {
        val (action, value) = instruction
        when (action) {
            'N' -> coord += Direction.N * value
            'S' -> coord += Direction.S * value
            'E' -> coord += Direction.E * value
            'W' -> coord += Direction.W * value
            'L' -> direction = direction.turn(-value)
            'R' -> direction = direction.turn(value)
            'F' -> coord += direction * value
            else -> error("unknown action $action. instruction $instruction")
        }
        debug { "After $instruction: $this" }
    }

    override fun toString(): String = "Ship at $coord heading $direction"
}

class Ship2 {
    var coord = Coord(0, 0)
    var waypoint = Vector(10, 1) // relative to the ship

    fun apply(instruction: Instruction) {
        val (action, value) = instruction
        when (action) {
            'N' -> waypoint += Direction.N * value
            'S' -> waypoint += Direction.S * value
            'E' -> waypoint += Direction.E * value
            'W' -> waypoint += Direction.W * value
            'L' -> waypoint = waypoint.rotateLeft(value)
            'R' -> waypoint = waypoint.rotateLeft(-value)
            'F' -> coord += waypoint * value
            else -> error("unknown action $action. instruction $instruction")
        }
    }
}

data class Vector(val x: Int, val y: Int) {
    operator fun plus(v: Vector): Vector = Vector(x + v.x, y + v.y)
    operator fun times(dist: Distance): Vector = Vector(dist*x, dist*y)

    fun rotateLeft(degrees: Angle): Vector = when(degrees.plus(360).div(90).mod(4)) {
        0 -> this
        1 -> Vector(-y, x)
        2 -> Vector(-x, -y)
        3 -> Vector(y, -x)
        else -> error("could not rotate $this $degrees degrees left")
    }
}

data class Coord(val x: Int, val y: Int) {
    operator fun plus(v: Vector): Coord = Coord(x + v.x, y + v.y)
    fun manhattan(): Int = x.absoluteValue + y.absoluteValue
}

enum class Direction(val dx: Int, val dy: Int) {
    N(0, 1),
    E(1, 0),
    S(0, -1),
    W(-1, 0);

    operator fun times(dist: Distance): Vector = Vector(dx*dist, dy*dist)
    fun turn(degrees: Angle): Direction = degrees.plus(360).div(90).plus(ordinal).mod(4)
        .also { debug { "Rotating $this by $degrees degrees" } }
        .let { values()[it] }
}

