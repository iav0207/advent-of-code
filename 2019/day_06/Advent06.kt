package advent2019.day06

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val moons: Map<String, List<String>> = generateSequence { readLine()?.trimEnd() }.toList()
        .map { val s = it.split(")"); s[0] to s[1] }
        .groupBy({ it.first }, { it.second })
    val suns: Map<String, String> = moons.toList().flatMap { (c, os) -> os.map { it to c } }.toMap()

    var totalOrbits = 0
    val seenMoons = mutableSetOf<String>()

    fun visit(body: String, level: Int = 0) {
        seenMoons.add(body)
        totalOrbits += level
        for (moon in moons[body] ?: emptyList()) {
            if (moon in seenMoons) continue
            visit(moon, level + 1)
        }
    }

    visit("COM")

    println("Part 1: $totalOrbits")

    val (start, target) = suns["YOU"]!! to suns["SAN"]!!
    val seen = mutableSetOf<String>()

    fun seek(curr: String, step: Int = 0): Int {
        if (curr == target) return step
        seen.add(curr)
        val nexts = mutableListOf<String>().apply {
            moons[curr]?.also { addAll(it) }
            suns[curr]?.also { add(it) }
        }
        for (next in nexts) {
            if (next !in seen) {
                val found = seek(next, step + 1)
                if (found >= 0) return found
            }
        }
        return -1
    }

    println("Part 2: ${seek(start)}")
}

