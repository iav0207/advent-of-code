package advent2021.day17

import kotlin.math.*


var debug = false
fun debug(a: Any) = if (debug) println(a) else Unit
fun debug(a: () -> Any) = if (debug) println(a()) else Unit


fun main(args: Array<String>) {
    args.find { it == "-d" }?.also { debug = true }
    val target: Area = readLine()!!.split(":")[1].split(",")
        .map { s -> s.split("=")[1].split("..").map { it.toInt() } }
        .map { it[0] to it[1] }
        .let { Area(it.first().first..it.first().second, it.last().first..it.last().second) }

    val vx0min = sqrt(target.xr.min().toDouble()/2).roundToInt()
    val vx0max = target.xr.max()
    val vy0min = target.yr.min()
    val vy0max = -vy0min

    val winners = mutableSetOf<Velo>()
    var launches = 0
    var maxY = 0

    for (vx0 in vx0min..vx0max) {
        for (vy0 in vy0min..vy0max) {
            Launch(Velo(vx0, vy0)).takeIf { it.reaches(target) }?.also {
                winners.add(it.v0)
                maxY = max(maxY, it.maxY)
            }
            launches += 1
        }
    }

    debug { winners.joinToString("\n", prefix = "Possible starting velocities are:\n") }

    println("Simulated $launches launches. Max Y = $maxY")
    println("Found ${winners.size} possible starting velocities.")
}

class Launch(val v0: Velo) {
    var maxY: Int = 0

    fun reaches(target: Area): Boolean {
        val maxSteps = -3 * target.yr.min() // assuming y range min is negative
        var p = Point(0, 0)
        var v = v0
        for (i in 0..maxSteps) {
            p = Point(p.x + v.vx, p.y + v.vy)
            v = Velo(v.vx - v.vx.sign(), v.vy - 1)
            maxY = max(maxY, p.y)
            if (p in target) return true
        }
        return false
    }
}

data class Point(val x: Int, val y: Int)
data class Velo(val vx: Int, val vy: Int)
data class Area(val xr: IntRange, val yr: IntRange) {
    operator fun contains(p: Point): Boolean = xr.contains(p.x) && yr.contains(p.y)
}

private fun IntRange.min(): Int = min(first, last)
private fun IntRange.max(): Int = max(first, last)

private fun Int.sign(): Int = sign(toDouble()).toInt()
