package advent2020.day19

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readLine()?.trimEnd() }.toList()

    debug { input }

    val unparsedRules: Map<String, String> = input.filter { ":" in it }
        .associate { it.substringBefore(":") to it.substringAfter(": ") }

    val rules: MutableMap<String, Rule> = mutableMapOf()

    fun parseRule(id: String): Rule {
        if (id in rules) return rules[id]!!
        val ruleStr = unparsedRules[id] ?: id // !!!
        return when {
            "\"" in ruleStr -> SimpleRule(id, ruleStr[1])
            "|" in ruleStr -> OrRule(id, ruleStr.split(" | ").map { parseRule(it) })
            else -> SeqRule(id, ruleStr.split(" ").map { parseRule(it) })
        }.also { if (" " !in id) rules.put(id, it) }
    }
    val rule0 = parseRule("0")
    debug { rules }

    val messages = input.filter { ":" !in it && it.isNotEmpty() }
    debug { messages }

    val matchCount = messages.count { msg ->
        rule0.matchAndAdvance(msg).any { adv -> run { adv == msg.length }.also { debug { "final match '$msg' adv $adv" } } }
    }

    print("Part 1: $matchCount")
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

class OrRule(override val id: String, private val subrules: Collection<Rule>) : Rule {
    override fun matchAndAdvance(str: String): Sequence<Int> {
        return subrules.asSequence()
            .flatMap { it.matchAndAdvance(str) }
            .distinct()
            .onEach { debug { "OrRule $id matches '$str', advance $it" } }
    }

    override fun toString() = "OrRule $id"
}

class SeqRule(override val id: String, private val subrules: List<Rule>) : Rule {
    override fun matchAndAdvance(str: String): Sequence<Int> {
        debug { "Matching SeqRule $id against '$str'" }
        return matchAndAdvance(str, 0, 0).distinct()
    }

    private fun matchAndAdvance(str: String, ruleNum: Int, cursor: Int) : Sequence<Int> {
        if (cursor > str.length) return sequenceOf()
        if (ruleNum in subrules.indices) {
            return subrules[ruleNum].matchAndAdvance(str.substring(cursor)).flatMap { advance ->
                matchAndAdvance(str, ruleNum + 1, cursor + advance)
            }.distinct().onEach { debug { "SeqRule $id matches '$str', advance $it " } }
        }
        return sequenceOf(cursor)
    }

    override fun toString() = "SeqRule $id"
}

