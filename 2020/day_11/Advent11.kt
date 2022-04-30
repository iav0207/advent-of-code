package advent2020.day11

var part = 1

fun main(vararg args: String) {
    part = if ("2" in args) 2 else 1

    var left: Field = readField()
    var right: Field = left.copy()

    do {
        left.step(right)
        right.step(left)
    } while (!left.isEqualTo(right))

    val occupied = left.sumOf { it.count { c -> c.isOccupied() } }
    println("Part $part: After the chaos stabilizes there are $occupied occupied seats.")
}

typealias Field = Array<CharArray>
data class Cell(val x: Int, val y: Int)

fun readField(): Field = generateSequence { readLine()?.trimEnd() }
    .map { it.toCharArray() }
    .toList()
    .toTypedArray()

fun Field.copy() = Array(size) { get(it).copyOf() }

fun Field.step(next: Field) = cells()
    .filter { get(it).isSeat() }
    .forEach { c ->
        val seat = get(c)
        val neighbours = if (part == 1) adjacent(c) else surrounding(c)
        val neiOccupiedCount = neighbours.count { get(it).isOccupied() }
        val thresh = if (part == 1) 4 else 5
        next[c] = when {
            seat.isEmpty() && neiOccupiedCount == 0 -> '#'
            seat.isOccupied() && neiOccupiedCount >= thresh -> 'L'
            else -> seat
        }
    }

fun Field.cells(): Sequence<Cell> = indices.flatMap { x ->
    get(0).indices.map { y -> Cell(x, y) }
}.asSequence()

fun Field.adjacent(cell: Cell): Sequence<Cell> = directions
    .map { (dx, dy) -> Cell(cell.x + dx, cell.y + dy) }
    .filter { it in this }

fun Field.surrounding(cell: Cell): Sequence<Cell> = directions.mapNotNull { (dx, dy) ->
    cell.beam(dx, dy)
        .takeWhile { it in this }
        .find { get(it).isSeat() }
}

val directions = sequenceOf(
    Pair(-1, -1),
    Pair(-1,  0),
    Pair(-1,  1),
    Pair( 0, -1),

    Pair( 0,  1),
    Pair( 1, -1),
    Pair( 1,  0),
    Pair( 1,  1),
)

fun Cell.beam(dx: Int, dy: Int): Sequence<Cell> = sequence {
    var (xx, yy) = Pair(x, y)
    while (true) {
        xx += dx
        yy += dy
        yield(Cell(xx, yy))
    }
}

fun Char.isEmpty() = this == 'L'
fun Char.isOccupied() = this == '#'
fun Char.isSeat() = this in "L#"

operator fun Field.get(cell: Cell): Char = get(cell.x).get(cell.y)
operator fun Field.set(cell: Cell, value: Char) = Unit.also { get(cell.x)[cell.y] = value }
operator fun Field.contains(cell: Cell) = cell.x in indices && cell.y in get(0).indices

fun Field.isEqualTo(o: Any): Boolean {
    if (o !is Array<*>) return false
    val of = (o as? Array<*>) ?: return false
    if (size != of.size) return false
    for (x in indices) {
        val orow = of[x]
        if (orow !is CharArray) return false
        if (get(x).size != orow.size) return false
        for (y in get(x).indices) {
            if (get(x)[y] != orow[y]) return false
        }
    }
    return true
}

