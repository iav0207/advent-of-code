import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

debug = "-d" in args

data class Payload(val id: Int, val value: Long)

class Node(
    var payload: Payload,
    var prev: Node? = null,
    var next: Node? = null,
) {
    fun prev(): Node = prev!!
    fun next(): Node = next!!
    fun swapWith(other: Node): Node {
        val otherPayload = other.payload
        other.payload = payload
        payload = otherPayload
        return other
    }
    fun asSequence(): Sequence<Node> = generateSequence(this) { it.next() }
}

val input: List<Long> = generateSequence { readLine()?.trimEnd()?.toLong() }.toList()
val len = input.size

fun buildRing(): Collection<Node> {
    val ring = input.withIndex()
        .map { (i, it) -> Payload(i, it) }
        .map { Node(it) }
    ring.zipWithNext().forEach { (prev, next) ->
        prev.next = next
        next.prev = prev
    }
    ring.first().prev = ring.last()
    ring.last().next = ring.first()
    return ring
}

val mix = buildRing()

debug { mix.first().asSequence().take(len).map { it.payload.value }.toList() }

for (id in 0 until len) {
    val start = mix.find { node -> node.payload.id == id }!!
    val num = start.payload.value
    fun step(node: Node) = if (num >= 0L) node.swapWith(node.next()) else node.swapWith(node.prev())
    var toGo = Math.abs(num)
    var node = start
    while (toGo-- > 0) node = step(node)
    debug { mix.first().asSequence().take(len).map { it.payload.value }.toList() }
}

val zeroNode = mix.find { it.payload.value == 0L }!!
var p1 = 0L
var cur = zeroNode
for (i in 1..3000) {
    cur = cur.next()
    if (i % 1000 == 0) p1 += cur.payload.value
}
println("Part 1: $p1")

