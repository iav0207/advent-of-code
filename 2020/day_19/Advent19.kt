package advent2020.day19

import java.util.ArrayDeque
import java.util.Deque

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

var part = 1

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readLine()?.trimEnd() }.toList()

    debug { input }

    val unparsedRules: Map<String, String> = input.filter { ":" in it }
        .associate { it.substringBefore(":") to it.substringAfter(": ") }
        .toMutableMap()

    val rules = parse(unparsedRules)

    val rule0 = rules["0"]!!
    debug { rules }

    val messages = input.filter { ":" !in it && it.isNotEmpty() }
    debug { messages }

    val matchCount = messages.count { msg ->
        rule0.matchAndAdvance(msg).any { adv -> run { adv == msg.length }.also { debug { "final match '$msg' adv $adv" } } }
    }

    print("Part 1: $matchCount")
}

fun parse(unparsed: Map<String, String>): Map<String, Rule> {
    val parsed: MutableMap<String, Rule> = mutableMapOf()
    val ruleGetter: (ID) -> Rule = { parsed[it]!! }

    fun parseRule(id: String): Rule {
        if (id in parsed) return parsed[id]!!
        val ruleStr = unparsed[id] ?: id // !!!
        debug { "Parsing $ruleStr" }
        return when {
            "\"" in ruleStr -> SimpleRule(id, ruleStr[1])
            "|" in ruleStr -> {
                ruleStr.split(" | ")
                    .filter { " " in it }
                    .forEach { compositeId -> // ephemeral "composite ids" like `14 2`
                        parsed[compositeId] =
                            SeqRule(compositeId, compositeId.split(" "), ruleGetter)
                    }
                OrRule(id, ruleStr.split(" | "), ruleGetter)
            }
            else -> SeqRule(id, ruleStr.split(" "), ruleGetter)
        }
    }

    val toParse: Deque<ID> = unparsed.keys.toCollection(ArrayDeque())
    while (toParse.isNotEmpty()) {
        val curr = toParse.pop()
        try {
            parsed[curr] = parseRule(curr)
        } catch (ex: NullPointerException) {
            toParse.add(curr)
        }
    }
    return parsed
}

interface Rule {
    val id: String
    fun matchAndAdvance(str: String): Sequence<Int>
}

class SimpleRule(override val id: String, private val char: Char) : Rule {
    override fun matchAndAdvance(str: String): Sequence<Int> = if (match(str)) sequenceOf(1) else sequenceOf()

    private fun match(str: String) = str.startsWith(char).also {
        debug { if (it) "$this matches $str advance 1" else "$this does not match $str" }
    }


    override fun toString() = "SimpleRule $id"
}

class OrRule(override val id: String,
             private val subrules: Collection<ID>,
             private val getRule: (ID) -> Rule,
) : Rule {
    override fun matchAndAdvance(str: String): Sequence<Int> {
        return subrules.asSequence()
            .map(getRule)
            .flatMap { it.matchAndAdvance(str) }
            .distinct()
            .onEach { debug { "OrRule $id matches '$str', advance $it" } }
    }

    override fun toString() = "OrRule $id"
}

class SeqRule(override val id: String,
              private val subrules: List<ID>,
              private val getRule: (ID) -> Rule,
) : Rule {
    override fun matchAndAdvance(str: String): Sequence<Int> {
        debug { "Matching SeqRule $id against '$str'" }
        return matchAndAdvance(str, 0, 0).distinct()
    }

    private fun matchAndAdvance(str: String, ruleNum: Int, cursor: Int) : Sequence<Int> {
        if (cursor > str.length) return sequenceOf()
        if (ruleNum in subrules.indices) {
            return subrules[ruleNum]
                .let(getRule)
                .matchAndAdvance(str.substring(cursor))
                .flatMap { advance -> matchAndAdvance(str, ruleNum + 1, cursor + advance) }
                .distinct()
                .onEach { debug { "SeqRule $id matches '$str', advance $it " } }
        }
        return sequenceOf(cursor)
    }

    override fun toString() = "SeqRule $id"
}

