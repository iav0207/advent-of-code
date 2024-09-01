package advent2020.day20

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val tiles = generateSequence { readlnOrNull()?.trimEnd() }.toList().splitOn("")
        .map { tileLines -> Tile(
            id = tileLines.first().drop(5).take(4).toLong(),
            body = tileLines.drop(1).parseBody(),
        )}.debug { "parsed ${it.size} tiles" }
        .associateBy { it.id }

    val matches = mutableMapOf<ID, MutableSet<Match>>().apply {
        tiles.values.forEach { thisTile ->
            tiles.values.filter { it.id != thisTile.id }
                .flatMap { listOf(it, it.flip()) }
                .flatMap { generateSequence(it) { it.rotate() }.take(4) }
                .forEach { other ->
                    direcs.filter { thisTile.linesUpWith(other, it) }
                        .forEach { direc -> getOrPut(thisTile.id) { mutableSetOf() }.add(Match(thisTile, direc, other)) }
                }
        }
    }.debug { it.entries.joinToString("\n") { (k, v) -> "$k: ${v.size}" } }

    matches.filterValues { it.size == 2 }
        .keys.fold(1L) { it, acc -> it * acc }
        .also { println("Part 1: $it") }

    // zeroTile is the tile at (0, 0) offset, top left.
    val zeroTile = tiles[matches.filterValues { it.map { m -> m.side }.toSet() == setOf(DOWN, RIGHT) }.keys.first()]!!

    val img = buildImage(zeroTile, matches).debug { it.format() }
    Body(side = img.sideLength, img = img)
        .let { sequenceOf(it, it.flip(true)) }
        .flatMap { b -> generateSequence(b) { it.rotate() }.take(4) }
        .map { it.img to it.img.findMonsters() }
        .find { (_, monsters) -> monsters.isNotEmpty() }!!
        .let { (image, monsters) -> image to monsters.flatMap { it } }
        .let { (image, mUnion) -> image.filterNot { it in mUnion } }
        .size.also { println("Part 2: $it") }
}

val monster: Image = listOf(
    listOf(18),
    listOf(0, 5, 6, 11, 12, 17, 18, 19),
    listOf(1, 4, 7, 10, 13, 16),
).withIndex().map { (i, js) -> js.map { Vec(i, it) } }.flatMap { it }.toSet()

typealias Image = Set<Vec>

fun Image.findMonsters(): Set<Image> = allVecs()
    .map { start -> monster.map { it + start }.toSet() } // offsetting the monster to all possible positions
    .filter { containsAll(it) }
    .toSet()

fun Image.print(grid: Int = 100) = println(format(grid))
fun Image.format(grid: Int = 100): String {
    val sb = StringBuilder()
    for (i in iMin..iMax) {
        if (i % grid == 0) sb.appendLine()
        for (j in jMin..jMax) {
            if (j % grid == 0) sb.append(" ")
            sb.append(if (Vec(i, j) in this) "#" else " ")
        }
        sb.appendLine()
    }
    return sb.toString()
}
fun Image.allVecs(): Sequence<Vec> = (iMin..iMax).asSequence().flatMap { i -> (jMin..jMax).map { j -> Vec(i, j) } }
fun Image.hashes(): Sequence<Vec> = asSequence()
val Image.sideLength: Int get() = max(iMax - iMin, jMax - jMin)
val Image.iMax: Int get() = maxOf { it.i }
val Image.iMin: Int get() = minOf { it.i }
val Image.jMax: Int get() = maxOf { it.j }
val Image.jMin: Int get() = minOf { it.j }

fun buildImage(zeroTileOriginal: Tile, matches: Map<ID, Set<Match>>): Image { // TODO refactor it heavily
    val zeroTile = zeroTileOriginal.dropBorders()
    val img = zeroTile.body.img.toMutableSet()
    val offsets = mutableMapOf<ID, Vec>(zeroTile.id to Vec(0, 0))
    val absoluteConfigs = mutableMapOf<ID, Config>(zeroTile.id to zeroTile.config)
    val tiles = mutableMapOf<ID, Tile>(zeroTile.id to zeroTile)

    fun add(match: Match) {
        match.debug { "add match base=${it.base.id} pair=${it.pair.id} side=${it.side}\n\tpaircfg=${it.pair.config}" }
        val baseOffset = offsets[match.base.id]!!.debug { "\tbaseOffset = $it" }
        val baseConfig = absoluteConfigs[match.base.id]!!.debug { "\tbaseAbsCfg = $it" }

        val absPairConfig = match.pair.config.repeatOpsAfter(baseConfig)
        absPairConfig.debug { "\tpairAbsCfg = $it" }
        val pairTile = match.pair.orientAs(absPairConfig).dropBorders()

        // The side should get rotated according to the orientation of the base tile.
        val side = baseConfig.ops().fold(match.side) { vec, op -> vec.apply(op) }

        val sideLength = pairTile.body.side
        val pairOffset = side.times(sideLength).plus(baseOffset)
            .debug { "\tpairOffset = $it, side = $side, sideLength = $sideLength" }

        debug { "\tmatch.side=${match.side}" }
        debug { "\tabsSide=$side" }
        pairTile.body.img.map { it + pairOffset }.forEach { img.add(it) }
        tiles[pairTile.id] = pairTile
        check(pairOffset !in offsets.values) { "duplicate pair offset=$pairOffset for tile ${pairTile.id}" }
        offsets[pairTile.id] = pairOffset
        absoluteConfigs[pairTile.id] = absPairConfig
    }

    val todo = ArrayDeque<ID>().apply { add(zeroTile.id) }
    while (todo.isNotEmpty()) {
        val baseId = todo.removeFirst()
        matches[baseId]
            ?.filter { it.pair.id !in tiles }
            ?.onEach { add(it) }
            ?.forEach { todo.addLast(it.pair.id) }
    }
    debug { offsets }
    return img
}

fun Tile.dropBorders() = copy(body = body.dropBorders())
fun Body.dropBorders() = copy(side = side - 2, img = img.filterNot { it.i == 0 || it.j == 0 || it.i == side - 1 || it.j == side - 1 }.toSet())
fun Config.rotationOffset() = index.rotationOffset()
fun Config.isFlipped() = index.rotate() == thumb

fun Direc.rotationOffset() = direcs.indexOf(this)

fun Tile.orientAs(config: Config): Tile = sequenceOf(this, flip())
    .flatMap { tile -> generateSequence(tile) { it.rotate() }.take(4) }
    .find { it.config == config }!!

enum class Op { ROTA, HFLIP, VFLIP }
fun Config.ops(): List<Op> = mutableListOf<Op>().apply {
    val rotas = index.rotationOffset()
    repeat(rotas) { add(Op.ROTA) }
    val isThumbHorizontal = thumb == RIGHT || thumb == LEFT
    fun thumbAt(offset: Int) = RIGHT.rotateTimes(offset)
    val isFlipped = thumb != thumbAt(rotas)
    if (isFlipped) add(if (isThumbHorizontal) Op.HFLIP else Op.VFLIP)
}
fun Config.repeatOpsAfter(other: Config) = other.ops().fold(this) { c, op -> c.apply(op) }
fun Config.apply(op: Op) = when(op) {
    Op.ROTA -> rotate()
    Op.HFLIP -> hflip()
    Op.VFLIP -> vflip()
}
fun Vec.apply(op: Op) = when(op) {
    Op.ROTA -> rotate()
    Op.HFLIP -> hflip()
    Op.VFLIP -> vflip()
}

typealias ID = Long
data class Tile(val id: ID, val body: Body, val config: Config = Config())
data class Config(val index: Vec = UP, val thumb: Vec = RIGHT)
data class Body(val side: Int, val img: Image) {
    override fun toString() = img.format()
}
data class Vec(val i: Int, val j: Int) {
    override fun toString() = when (this) {
        UP -> "UP"
        LEFT -> "LEFT"
        DOWN -> "DOWN"
        RIGHT -> "RIGHT"
        else -> "Vec($i, $j)"
    }
}
data class Match(val base: Tile, val side: Direc, val pair: Tile)

typealias Direc = Vec

val UP = Direc(-1, 0)
val DOWN = Direc(1, 0)
val LEFT = Direc(0, -1)
val RIGHT = Direc(0, 1)
val direcs = listOf(UP, LEFT, DOWN, RIGHT)

fun Tile.flip() = copy(body = body.flip(config.willFlipHorizontally()), config = config.flip())
fun Body.flip(horizontal: Boolean) = if (horizontal) hflip() else vflip()
fun Body.hflip() = copy(img = img.map { it.hflip(side) }.toSet() )
fun Body.vflip() = copy(img = img.map { it.vflip(side) }.toSet() )

fun Config.flip() = copy(thumb = thumb.flip())
fun Config.hflip() = copy(index = index.hflip(), thumb = thumb.hflip())
fun Config.vflip() = copy(index = index.vflip(), thumb = thumb.vflip())
fun Config.willFlipHorizontally() = thumb == RIGHT || thumb == LEFT

fun Vec.flip() = -this
fun Vec.hflip(side: Int = 1) = copy(j = side - 1 - j)
fun Vec.vflip(side: Int = 1) = copy(i = side - 1 - i)

fun Tile.rotate() = copy(config = config.rotate(), body = body.rotate())
fun Body.rotate() = copy(img = img.map { it.rotate(side) }.toSet())
fun Config.rotate() = copy(index = index.rotate(), thumb = thumb.rotate())
fun Vec.rotate(side: Int = 1) = Vec(side - 1 - j, i) // plane rotation formula
fun Direc.rotateTimes(n: Int) = generateSequence(this) { it.rotate() }.drop(n).first()

fun Tile.linesUpWith(other: Tile, onSide: Direc) = body.project(onSide) == other.body.project(onSide.flip())
fun Body.project(direc: Direc): Set<Int> = indices.map {
    when(direc) {
        UP -> Vec(0, it)
        LEFT -> Vec(it, 0)
        DOWN -> Vec(side - 1, it)
        RIGHT -> Vec(it, side - 1)
        else -> error("cannot project side $direc")
    }
}.filter { it in img }.map { if (direc.i == 0) it.i else it.j }.toSet()

val Body.indices get() = 0 until side

operator fun Vec.plus(o: Vec) = Vec(i + o.i, j + o.j)
operator fun Vec.minus(o: Vec) = plus(-o)
operator fun Vec.unaryMinus() = Vec(-i, -j)
operator fun Vec.times(n: Int) = Vec(n*i, n*j)

fun <T> List<T>.splitOn(separator: T): List<List<T>> = mutableListOf<List<T>>().also { result ->
    var left = this
    while (left.isNotEmpty()) {
        result.add(left.takeWhile { it != separator })
        left = left.drop(result.last().size + 1)
    }
}

fun List<String>.parseBody() = Body(side = size, img = mutableSetOf<Vec>().also { img ->
    indices.flatMap { i -> indices.map { j -> Vec(i, j) } }
        .filter { get(it.i).get(it.j) == '#' }
        .forEach { img.add(it) }
})
