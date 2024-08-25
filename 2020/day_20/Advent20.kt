package advent2020.day20

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val tiles = generateSequence { readlnOrNull()?.trimEnd() }.toList().splitBy("")
        .map { tileLines -> Tile(
            id = tileLines.first().drop(5).take(4).toLong(),
            body = tileLines.drop(1).parseBody(),
        )}.debug { "parsed ${it.size} tiles" }

    val matches = mutableMapOf<ID, MutableSet<Match>>().apply {
        tiles.forEach { thisTile ->
            tiles.filter { it.id != thisTile.id }
                .flatMap { listOf(it, it.flip()) }
                .flatMap { generateSequence(it) { it.rotate() }.take(4) }
                .forEach { other ->
                    direcs.filter { thisTile.linesUpWith(other, it) }
                        .forEach { getOrPut(thisTile.id) { mutableSetOf() }.add(Match(thisTile.id, other.id, it, other.config)) }
                }
        }
    }.debug { it.entries.joinToString("\n") { (k, v) -> "$k: $v" } }

    matches.filterValues { it.size == 2 }
        .keys.fold(1L) { it, acc -> it * acc }
        .also { println("Part 1: $it") }
}

fun List<String>.parseBody() = Body(side = size, f = mutableSetOf<Point>().also { f ->
    indices.flatMap { i -> indices.map { j -> Point(i, j) } }
        .filter { get(it.i).get(it.j) == '#' }
        .forEach { f.add(it) }
})

typealias ID = Long
data class Tile(val id: ID, val body: Body, val config: Config = Config())
data class Config(val index: Point = UP, val thumb: Point = RIGHT)
data class Body(val side: Int, val f: Set<Point>)
data class Point(val i: Int, val j: Int)
data class Match(val base: ID, val pair: ID, val side: Direc, val pairConfig: Config)
typealias Direc = Point

val UP = Direc(-1, 0)
val DOWN = Direc(1, 0)
val LEFT = Direc(0, -1)
val RIGHT = Direc(0, 1)
val direcs = listOf(UP, LEFT, DOWN, RIGHT)

fun Tile.flip() = copy(body = body.flip(config.willFlipHorizontally()), config = config.flip())
fun Body.flip(horizontal: Boolean) = copy(f = f.map { Point(
    i = if (horizontal) it.i else side - it.i - 1,
    j = if (horizontal) side - it.j - 1 else it.j,
)}.toSet())
fun Config.flip() = copy(thumb = thumb.flip())
fun Point.flip() = -this
fun Config.willFlipHorizontally() = thumb == RIGHT || thumb == LEFT

fun Tile.rotate() = copy(config = config.rotate(), body = body.rotate())
fun Body.rotate() = copy(f = f.map { it.rotate(side) }.toSet())
fun Config.rotate() = copy(index = index.rotate(), thumb = thumb.rotate())
fun Point.rotate(side: Int = 1) = Point(side - 1 - j, i) // plane rotation formula

fun Tile.linesUpWith(other: Tile, onSide: Direc) = body.project(onSide) == other.body.project(onSide.flip())
fun Body.project(direc: Direc): Set<Int> = indices.map {
    when(direc) {
        UP -> Point(0, it)
        RIGHT -> Point(it, 0)
        DOWN -> Point(side - 1, it)
        LEFT -> Point(it, side - 1)
        else -> error("cannot project side $direc")
    }
}.filter { it in f }.map { if (direc.i == 0) it.i else it.j }.toSet()

val Body.indices get() = 0 until side

operator fun Point.plus(o: Point) = Point(i + o.i, j + o.j)
operator fun Point.minus(o: Point) = plus(-o)
operator fun Point.unaryMinus() = Point(-i, -j)

fun <T> List<T>.splitBy(separator: T): List<List<T>> = mutableListOf<List<T>>().also { result ->
    var left = this
    while (left.isNotEmpty()) {
        result.add(left.takeWhile { it != separator })
        left = left.drop(result.last().size + 1)
    }
}

