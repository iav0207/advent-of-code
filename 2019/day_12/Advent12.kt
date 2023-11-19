package advent2019.day12

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args

    val moons = generateSequence { readLine()?.trimEnd() }
        .map { it.parseVector().let { Body(it) } }
        .toList()

    val resultState = generateSequence(State(moons)) { it.next() }
        .take(1 + 1000)
        .onEachIndexed { i, state -> debug { "$i. $state | total energy = ${state.totalEnergy()}" } }
        .last()

    println("Part 1: ${resultState.totalEnergy()}")
}

fun String.parseVector(): Vec = numbersRegex.findAll(this)
    .map { it.value.toInt() }
    .toList()
    .run { Vec(get(0), get(1), get(2)) }

val numbersRegex = """(-?\d+)""".toRegex()

data class Body(val pos: Vec, val vel: Vec = Vec(0, 0, 0)) {
    fun totalEnergy() = potentialEnergy() * kineticEnergy()
    fun potentialEnergy() = pos.energy()
    fun kineticEnergy() = vel.energy()
}

data class Vec(val x: Int, val y: Int, val z: Int) {
    operator fun plus(o: Vec) = Vec(x + o.x, y + o.y, z + o.z)

    fun gravityTowards(o: Vec) = Vec(sign(o.x - x), sign(o.y - y), sign(o.z - z))
    fun energy(): Int = abs(x) + abs(y) + abs(z)
    private fun sign(n: Int): Int = sign(n.toDouble()).toInt()
}

fun Iterable<Vec>.sum(): Vec = reduce { acc, it -> acc + it }

data class State(val bodies: List<Body>) {
    val n = bodies.size

    fun next(): State {
        val newV = List(n) { i ->
            (0 until n).map { j -> bodies[i].pos.gravityTowards(bodies[j].pos) }.sum() + bodies[i].vel
        }
        val newP = bodies.map { it.pos }.zip(newV) { p, v -> p + v }
        return State(newP.zip(newV) { pos, vel -> Body(pos, vel) })
    }

    fun totalEnergy() = bodies.sumOf { it.totalEnergy() }
}

