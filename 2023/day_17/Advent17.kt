package advent2023.day17

import kotlin.math.*
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
        println("Part 1: ${part1()}")
        println("Part 2: ${part2()}")
    }
}

data class Vec(val i: Int, val j: Int)
operator fun Vec.plus(o: Vec) = Vec(i + o.i, j + o.j)
operator fun Vec.minus(o: Vec) = Vec(i - o.i, j - o.j)
fun Vec.inv() = Vec(-i, -j)

typealias Field = List<List<Int>>
typealias HeatLoss = Int

typealias Count = Int
data class Node(val pos: Vec, val direc: Vec, val lost: HeatLoss, val conseq: Count) {
    fun newChild(direc: Vec, loss: HeatLoss) = Node(
        pos = pos + direc,
        direc = direc,
        lost = lost + loss,
        conseq = 1 + if (direc == this.direc) conseq else 0,
    )
}

class Solution(private val field: Field) {
    private val start = Vec(0, 0)
    private val end = Vec(field.indices.last, field[0].indices.last)

    fun part1(): Int {
        val todo = PriorityQueue<Node>(compareBy { it.lost })
        val best = mutableMapOf<Vec, HeatLoss>()

        todo.add(Node(Vec(0, 0), Vec(0, 0), 0, 0))

        while (todo.isNotEmpty()) {
            val p = todo.remove()
            if (best[p.pos]?.let { it <= p.lost } == true) continue
            best[p.pos] = p.lost
            for (direc in allDirections) {
                val newPos = p.pos + direc
                if (newPos !in field) continue
                val child = p.newChild(direc, field[newPos])
                if (child.direc != p.direc.inv() && child.conseq < 4) todo.add(child)
            }
        }
        return best[end]!!
    }

    private val allDirections = setOf(Vec(0, 1), Vec(0, -1), Vec(1, 0), Vec(-1, 0))

    fun part2(): Int = 0

    operator fun Field.get(v: Vec) = get(v.i)[v.j]
    operator fun Field.contains(v: Vec) = v.i in indices && v.j in get(0).indices
}

