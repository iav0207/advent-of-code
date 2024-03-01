package advent2023.day19

import kotlin.math.*
import java.util.SortedSet

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val (items, workflows) = generateSequence { readlnOrNull()?.trimEnd() }
        .filter { it.isNotBlank() }
        .partition { it.startsWith("{") }
        .run { first.map { it.asItem() } to second.map { it.asWorkflow() }.associateBy { it.name } }

    items.filter { it.isAcceptedBy(workflows) }
        .sumOf { it.x + it.m + it.a + it.s }
        .also { println("Part 1: $it") }

    println("Part 2: ${countDistinctAcceptedItems(workflows)}")
}

fun Item.isAcceptedBy(wfs: Workflows): Boolean {
    var wfId = "in"
    while (wfId !in listOf("A", "R")) {
        val wf = wfs[wfId] ?: error("unknown workflow: $wfId")
        wfId = wf.branches.first { it.condition(this) }.goto
    }
    return wfId == "A"
}

fun countDistinctAcceptedItems(wfs: Workflows): Long {
    val range = 1..4000

    val (bx, bm, ba, bs) = wfs.bounds(range)
        .run { listOf(get('x')!!, get('m')!!, get('a')!!, get('s')!!) }

    fun nextAlong(v: Val, dimBounds: SortedSet<Val>) =
        dimBounds.tailSet(v).drop(1).firstOrNull()?.minus(1) ?: range.last()

    var count = 0L
    for (x1 in bx) {
        val x2 = nextAlong(x1, bx)
        for (m1 in bm) {
            val m2 = nextAlong(m1, bm)
            for (a1 in ba) {
                val a2 = nextAlong(a1, ba)
                for (s1 in bs) {
                    val s2 = nextAlong(s1, bs)
                    val accepted = Item(x1, m1, a1, s1).isAcceptedBy(wfs)
                    debug { "[$x1..$x2, $m1..$m2, $a1..$a2, $s1..$s2]: $accepted" }
                    if (accepted) {
                        val volume = (x2 - x1 + 1).toLong() * (m2 - m1 + 1) * (a2 - a1 + 1) * (s2 - s1 + 1)
                        count += volume.debug { "volume = $it" }
                    }
                }
            }
        }
    }
    return count
}

fun Workflows.bounds(range: IntRange): MutableMap<Char, SortedSet<Val>> {
    val bounds: MutableMap<Char, SortedSet<Val>> = "xmas".associateWith { sortedSetOf(1) }.toMutableMap()
    values
        .flatMap { it.branches }
        .map { it.src }
        .filter { ":" in it }
        .forEach {
            val v1 = it.allInts()[0]
            val v2 = when {
                "<" in it && v1 > 1 -> v1 - 1
                ">" in it && v1 < range.last() -> v1 + 1
                else -> v1
            }
            val dim = it.first()
            bounds[dim]!!.apply { add(v1); add(v2) }
        }
    return bounds
}

fun String.asItem() = allInts().run { Item(get(0), get(1), get(2), get(3)) }
fun String.allInts() = intsRegex.findAll(this).map { it.value.toInt() }.toList()
val intsRegex = "\\d+".toRegex()

fun String.asWorkflow() = split("[{}]".toRegex()).run {
    Workflow(get(0), get(1).split(",").map { it.asBranch() })
}
fun String.asBranch() = when {
    ":" !in this -> Branch({ true }, this, this)
    else -> {
        check("<" in this || ">" in this) { "cannot parse branch: $this" }
        val constant = allInts()[0]
        val getProp = first().asGetter()
        Branch(
            condition = {
                val cmp = it.getProp().compareTo(constant)
                if ("<" in this) cmp < 0 else cmp > 0
            },
            goto = substringAfter(":"),
            src = this,
        )
    }
}
fun Char.asGetter(): Item.() -> Val = when(this) {
    'x' -> then { x }
    'm' -> then { m }
    'a' -> then { a }
    's' -> then { s }
    else -> error("unknown prop: " + this)
}
fun <T> then(x: T): T = x

data class Item(val x: Val, val m: Val, val a: Val, val s: Val)
typealias Val = Int

typealias Workflows = Map<String, Workflow>
class Workflow(val name: String, val branches: List<Branch>)
class Branch(val condition: Condition, val goto: String, val src: String)
typealias Condition = (Item) -> Boolean

