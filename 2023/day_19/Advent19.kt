package advent2023.day19

import kotlin.math.*

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
}

fun Item.isAcceptedBy(wfs: Map<String, Workflow>): Boolean {
    var wfId = "in"
    while (wfId !in listOf("A", "R")) {
        val wf: Workflow = wfs[wfId] ?: error("unknown workflow: $wfId")
        wfId = wf.branches.first { it.condition(this) }.goto
    }
    return wfId == "A"
}

fun String.asItem() = allInts().run { Item(get(0), get(1), get(2), get(3)) }
fun String.allInts() = intsRegex.findAll(this).map { it.value.toInt() }.toList()
val intsRegex = "\\d+".toRegex()

fun String.asWorkflow() = split("[{}]".toRegex()).run {
    Workflow(get(0), get(1).split(",").map { it.asBranch() })
}
fun String.asBranch() = when {
    ":" !in this -> Branch({ true }, this)
    else -> {
        check("<" in this || ">" in this) { "cannot parse branch: $this" }
        val getter = first().asProp()
        val constant = allInts()[0]
        Branch(
            condition = {
                val cmp = it.getter().compareTo(constant)
                if ("<" in this) cmp < 0 else cmp > 0
            },
            goto = substringAfter(":"),
        )
    }
}
fun Char.asProp(): Item.() -> Val = when(this) {
    'x' -> then { x }
    'm' -> then { m }
    'a' -> then { a }
    's' -> then { s }
    else -> error("unknown prop: " + this)
}
fun <T> then(x: T): T = x

data class Item(val x: Val, val m: Val, val a: Val, val s: Val)
typealias Val = Int

class Workflow(val name: String, val branches: List<Branch>)
class Branch(val condition: Condition, val goto: String)
typealias Condition = (Item) -> Boolean

