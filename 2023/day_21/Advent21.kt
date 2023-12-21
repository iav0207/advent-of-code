package advent2023.day21

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }.toList()
    Solution(input).apply {
        println("Part 1: ${part1()}")
        println("Part 2: ${part2()}")
    }
}

class Solution(private val fieldFragment: List<String>) {
    private val n = fieldFragment.size
    private val m = fieldFragment[0].length

    private val start = fieldFragment.indices
        .flatMap { i -> fieldFragment[i].indices.map { j -> Vec(i, j) } }
        .find { fieldFragment[it] == 'S' }!!

    private val cells: MutableMap<Offset, Cell> = mutableMapOf()
    private val cache: MutableMap<Pair<Offset, CellState>, Int> = mutableMapOf()
    private val cycles: MutableMap<Offset, Int> = mutableMapOf()

    fun part1(): Int = Cell(fieldFragment, setOf(start)).simulateLimited()
        .drop(64)
        .first().positions.size


    fun part2(): Long {
        val nSteps = 6
        cells[Offset(0, 0)] = Cell(fieldFragment, setOf(start))
        var toGo = nSteps
        while (toGo > 0) {
            for ((offset, cell) in cells.entries) {
                // if the cycle is known, then what?
                val outer = cell.makeStepUnlimitedGetOverflows().partition { it in fieldFragment }
                // translate vectors into neighbor cells' frames, engage the neighbors
                // if cached, insert the cycle if absent
                if (Pair(offset, cell.state.positions) in cache)
            }
            toGo--
        }
        return 0L
    }
}

class Cell(private val field: List<String>, initStep: CellState) {
    var state = Step(initStep)
    fun simulateLimited(): Sequence<Step> = generateSequence(state) { it.furtherIsolated() }.onEach { state = it }
    fun makeStepUnlimitedGetOverflows(): Set<Vec> = state.furtherUnlimited()
        .also { state = Step(it.filter { v -> v in field }.toSet()) }
        .filter { it !in field }
        .toSet()

    inner class Step(val positions: Set<Vec>) {
        fun furtherIsolated(): Step = positions.flatMap { p -> directions.map { p + it } }.toSet()
            .filter { it in field && field[it] != '#' }
            .run { Step(toSet()) }

        fun furtherUnlimited(): Set<Vec> = positions.flatMap { p -> directions.map { p + it } }.toSet()

        private val directions = listOf(Vec(1, 0), Vec(0, 1), Vec(-1, 0), Vec(0, -1))
    }
}
operator fun List<String>.contains(v: Vec) = v.i in field.indices && v.j in field[0].indices
operator fun List<String>.get(v: Vec) = field[v.i][v.j]

data class Vec(val i: Int, val j: Int)
operator fun Vec.plus(o: Vec) = Vec(i + o.i, j + o.j)

typealias Offset = Vec // cell offset
typealias CellState = Set<Vec>
