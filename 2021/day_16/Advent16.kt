package advent2021

import kotlin.math.max
import kotlin.math.min


const val DEBUG = false
fun debug(a: Any) = if (DEBUG) println(a) else Unit
fun debug(a: () -> Any) = if (DEBUG) println(a()) else Unit


fun main() {
    val bin: String = readLine()!!.toCharArray()
        .joinToString("") { it.toString().toInt(16).toString(2).padStart(4, '0') }

    val root = Packet(Reader(bin))

    println("Version numbers total is ${root.versionsSum()}")
    println("Expression evaluates to ${root.evaluate()}")
}

class Reader(private val binary: String, val c: Cursor = Cursor()) {
    fun readInt(n: Int): Int = readBin(n).toInt(2)
    fun read(n: Int): Long = readBin(n).toLong(2)
    fun readBin(n: Int): String = binary.substring(c.value, c.value + n).also { c.advanceBy(n) }.also { printBin() }
    fun left(): Int = binary.length - c.value

    fun createChild(len: Int) = Reader(readBin(len))

    fun printBin() = debug {
        with(StringBuilder()) {
            var pr = 0
            c.chunks.forEach { append(binary.substring(pr, pr + it)).append(' '); pr += it }
            append(binary.substring(pr)).toString()
        }
    }
}

class Cursor(var value: Int = 0, val chunks: MutableList<Int> = mutableListOf()) {
    fun advanceBy(n: Int) = chunks.add(n).also { value += n }
}

class Packet(val r: Reader) {
    val version = r.read(3)
    val typeId = r.readInt(3)
    val isLiteral = typeId == 4
    val label = if (isLiteral) "LITERAL" else "OPERATOR"

    init { debug { "$label version = $version typeID = $typeId" } }

    val nested = if (isLiteral) listOf() else readSubPackets()
    val value: Long? = if (isLiteral) readLiteral() else null

    fun versionsSum(): Long = version + nested.sumOf { it.versionsSum() }

    fun evaluate(): Long = value ?: run {
        if (nested.size > 1) debug { "Evaluate op=$typeId on ${nested.map { it.evaluate() }}" }
        nested.fold(-1L) { acc, b -> if (acc < 0) b.evaluate() else operation(acc, b.evaluate()) }
    }

    val operation: (Long, Long) -> Long = when(typeId) {
        0 -> { a, b -> a + b }
        1 -> { a, b -> a * b }
        2 -> { a, b -> min(a, b) }
        3 -> { a, b -> max(a, b) }
        4 -> { _, _ -> error("invoking operation on a literal?") }
        5 -> { a, b -> if (a > b) 1 else 0  }
        6 -> { a, b -> if (a < b) 1 else 0  }
        7 -> { a, b -> if (a == b) 1 else 0  }
        else -> error("yikes!")
    }

    fun readSubPackets(): List<Packet> {
        val lengthTypeId = r.readInt(1)
        return if (lengthTypeId == 0) {
            val totLen = r.readInt(15)
            debug { "Total length of contained subpackets is $totLen" }
            val subReader = r.createChild(totLen)

            mutableListOf<Packet>().apply {
                while (subReader.left() > 0) {
                    add(Packet(subReader))
                }
            }
        } else {
            val numSubp = r.readInt(11)
            debug { "Number of subpackets is $numSubp" }
            List(numSubp) { Packet(r) }
        }
    }

    fun readLiteral(): Long = with(StringBuilder()) {
        var read: Boolean
        do {
            read = r.read(1) == 1L
            append(r.readBin(4))
        } while (read)
        toString()
            .also { debug { "Read literal in binary: $it (${it.toLong(2)})" } }
            .toLong(2)
    }
}
