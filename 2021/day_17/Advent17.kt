package advent2021.day17

import java.util.*
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
    val pq = PriorityQueue<Launch>(compareBy { -it.score })
    val attempts: MutableSet<Velo> = mutableSetOf()

    fun enqueueNew(launch: Launch) = launch.takeIf { attempts.add(it.v0) }
        ?.also { pq.add(it) }
        ?.also { debug { "Launching ${it.v0} with score ${it.score}" } }

    var best = target.launch(Velo(5, 5)).also { enqueueNew(it) }
    var maxY = 0
    var was69launched = false
    val v69 = Velo(6, 9)
//    var lastScore = 0
    var lowScoreInRow = 0

    var i = 0
    while (pq.isNotEmpty() && i < 10000 && lowScoreInRow < 10000) {
        var launch = pq.remove()
        if (launch.v0 == v69) was69launched = true
        launch.simulate()
        lowScoreInRow += if (best.score > launch.score) 1 else -lowScoreInRow
        best = maxOf(best, launch, compareBy { it.score })
        maxY = maxOf(maxY, launch.maxY)
//        lastScore = launch.score
        debug { "$launch is at ${launch.position} dist ${target.distanceTo(launch.position)} score ${launch.score}" }

        launch.createCorrectedLaunches().forEach { enqueueNew(it) }
        i++
    }
    println("i=$i")
    println("Best is $best Max Y is $maxY was69=$was69launched")
}

class Launch(val v0: Velo, private val target: Area, var score: Int = 0) {
    private var v = v0
    var maxY: Int = 0
    var position: Point = Point(0, 0)
    var withinTarget: Boolean = position in target
    var lost: Boolean = false
    var minDist: Double = target.distanceTo(position)
    var i: Int = 0

    fun simulate() = repeat(20) { step() }

    fun step(): Launch? {
        if (withinTarget) return null
        debug { "$this: before step $i pos=$position v=$v score=$score" }
        position += v
        v = Velo(v.vx - v.vx.intSign(), v.vy - 1)
        maxY = max(maxY, position.y)
        withinTarget = position in target
        val dist = target.distanceTo(position)
        minDist = min(minDist, dist)
        score = score()
        debug { "$this: after step $i pos=$position v=$v dist=$dist score=$score reached=$withinTarget" }
        i += 1
        return this.takeUnless { withinTarget }
    }

    fun score(dist: Double = dist()) = (if (withinTarget) 1_000_000_000 else (1_000 - (dist*dist).roundToInt())) + maxY
    private fun dist() = target.distanceTo(position)

    fun createCorrectedLaunches(): Iterable<Launch> {
        val dx = cos(target.distanceTo(Point(position.x, 0)))
        val dy = sin(target.distanceTo(Point(0, position.y)))
        val childScore = score(minDist)
        return mutableListOf<Launch>().apply {
            for (i in 0..1) {
                for (j in 0..1) {
                    add(Launch(Velo(v0.vx + i, v0.vy - j), target, childScore))
                    add(Launch(Velo(v0.vx - i, v0.vy - j), target, childScore))
                }
            }
        }
    }

    override fun equals(other: Any?) = (other as? Launch)?.let { it.v0 == v0 } ?: false
    override fun hashCode(): Int = v0.hashCode()

    override fun toString() = "Launch with v0=$v0"
}

data class Point(val x: Int, val y: Int) {
    operator fun plus(v: Velo) = Point(x + v.vx, y + v.vy)
    operator fun minus(o: Point): Double {
        val dx = abs(x - o.x).toDouble()
        val dy = abs(y - o.y).toDouble()
        return sqrt(dx*dx + dy*dy)
    }
}
data class Velo(val vx: Int, val vy: Int)
data class Area(val xr: IntRange, val yr: IntRange) {
    val center: Point = Point(xr.center(), yr.center())
    val size: Double = Point(xr.max(), yr.max()) - center
    fun distanceTo(p: Point): Double = if (p in this) 0.0 else abs((p - center) - size)
    operator fun contains(p: Point): Boolean = xr.contains(p.x) && yr.contains(p.y)
    fun launch(v0: Velo) = Launch(v0, this)
}

private fun IntRange.center(): Int = min() + (max() - min())/2
private fun IntRange.min(): Int = min(first, last)
private fun IntRange.max(): Int = max(first, last)

private fun Int.intSign(): Int = toDouble().intSign()
private fun Double.intSign(): Int = sign(this).toInt()
