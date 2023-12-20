package advent2023.day17

import java.util.PriorityQueue

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }
        .map { line -> line.map { it.toString().toInt() } }
        .toList()

    Solution(input).apply {
        println("Part 1: ${solve(part2 = false)}")
        println("Part 2: ${solve(part2 = true)}")
    }
}

typealias Field = List<List<Int>>
typealias HeatLoss = Int
typealias Count = Int

data class Vec(val i: Int, val j: Int)
operator fun Vec.plus(o: Vec) = Vec(i + o.i, j + o.j)
operator fun Vec.minus(o: Vec) = Vec(i - o.i, j - o.j)
fun Vec.inv() = Vec(-i, -j)

class Solution(private val field: Field) {
    private val start = Node(Vec(0, 0), Vec(0, 0), 0, 0)
    private val endPos = Vec(field.indices.last, field[0].indices.last)

    fun solve(part2: Boolean): Int {
        val todo = PriorityQueue<Node>(compareBy { it.lost })
        val seen = mutableSetOf<Triple<Vec, Vec, Count>>()
        var best: HeatLoss? = null

        todo.add(start)

        while (todo.isNotEmpty())
            todo.remove()
                .takeIf { seen.add(it.key) }
                ?.apply { if (pos == endPos) best = minOf(best ?: lost, lost) }
                ?.children(part2)
                ?.forEach { todo.add(it) }

        return best!!
    }

    inner class Node(val pos: Vec, val direc: Vec, val lost: HeatLoss, val conseq: Count) {
        val key = Triple(pos, direc, conseq) // represents the state we only want to process once

        fun children(part2: Boolean) = allDirections
            .filter { pos + it in field }
            .map { newChild(it) }
            .filter { it.direc != direc.inv() }
            .filter { if (part2) filterPart2(it) else filterPart1(it) }

        private fun newChild(direc: Vec) = Node(
            pos = pos + direc,
            direc = direc,
            lost = lost + field[pos + direc],
            conseq = 1 + if (direc == this.direc) conseq else 0,
        )

        private fun filterPart1(child: Node) = child.conseq <= 3
        private fun filterPart2(child: Node) = child.conseq <= 10 && (pos == start.pos || direc == child.direc || conseq in 4..10)
    }
    private val allDirections = setOf(Vec(0, 1), Vec(0, -1), Vec(1, 0), Vec(-1, 0))

    operator fun Field.get(v: Vec) = get(v.i)[v.j]
    operator fun Field.contains(v: Vec) = v.i in indices && v.j in get(0).indices
}

