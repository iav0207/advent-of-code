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

    println("Part 2: ${PartTwo(State(moons)).run()}")
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

    override fun toString() = if (vel == Vec.ZERO) "{p=$pos}" else "{p=$pos v=$vel}"
}

data class Vec(val x: Int, val y: Int, val z: Int) {
    operator fun plus(o: Vec) = Vec(x + o.x, y + o.y, z + o.z)

    fun gravityTowards(o: Vec) = Vec(sign(o.x - x), sign(o.y - y), sign(o.z - z))
    fun energy(): Int = abs(x) + abs(y) + abs(z)
    private fun sign(n: Int): Int = sign(n.toDouble()).toInt()
    val manhattan get() = energy()

    override fun toString() = "[$x $y $z]"
    companion object {
        val ZERO = Vec(0, 0, 0)
    }
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
    val manhattan get() = bodies.sumOf { it.pos.manhattan }
}

const val DISCARDED = -1L // state marker: no need to evaluate the state

class PartTwo(private val initialState: State) {
    private val n = initialState.n
    private val dp = mutableMapOf<State, Long>() // map of a state to its cycle length

    private val manhattanLimit = 3 * initialState.manhattan

    fun run(): Long {
        val queue = mutableSetOf(State(List(n) { Body(Vec(0, 0, 0)) }))
        while (initialState !in dp && queue.isNotEmpty()) {
            val stateToEvaluate = queue.first().also { queue.remove(it) }
            val memo = mutableMapOf<State, Long>() // map of a state to the simulation step num
            memo[stateToEvaluate] = 0L
            var simulation = stateToEvaluate
            var i = 0L
            var discarded = false
            while (stateToEvaluate !in dp) {
                i++; simulation = simulation.next()
                // TODO limit not by manhattan, but by the distance from stateToEvaluate
                if (simulation.manhattan > manhattanLimit || dp[simulation] == DISCARDED) {
                    discarded = true
                    break
                }
                if (simulation in memo && simulation !in dp) {
                    val iPrev = memo[simulation]!!
                    dp[simulation] = i - iPrev
                }
                if (simulation in dp && stateToEvaluate !in dp) dp[stateToEvaluate] = i + dp[simulation]!!
                memo[simulation] = i
            }
            if (discarded) {
                memo.keys.forEach { dp[it] = DISCARDED }
            } else {
                advance(stateToEvaluate)
                    .onEach { queue.add(it) }
                    .last().also { if (i % 1000L == 0L) debug { "heading to $initialState\n        at $it" } }
            }
        }
        return dp[initialState]!!
    }

    private fun advance(s: State): Sequence<State> = sequence {
        for (i in s.bodies.indices) {
            val pos = s.bodies[i].pos
            val target = initialState.bodies[i].pos
            val delta = pos.gravityTowards(target)
            val nextStateBodies = s.bodies.toMutableList().also { it[i] = Body(pos + delta) }
            yield(State(nextStateBodies))
        }
    }
}

