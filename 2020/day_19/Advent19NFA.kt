package advent2020.day19

import advent2020.day19.Advent19NFA.State.ACCEPTED
import advent2020.day19.Advent19NFA.State.INTERMEDIATE
import advent2020.day19.Advent19NFA.State.REJECTED
import advent2020.day19.Advent19NFA.State.START

fun main() = Advent19NFA.main()

typealias ID = String

/**
 * Solves only part 1 correctly, because part 2 defines an irregular grammar.
 * I tried to apply a hack in this solution to extend it to part 2, but didn't manage to make it work.
 */
object Advent19NFA {
    var debug = false
    fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

    val matches: MutableMap<ID, MutableSet<String>> = sortedMapOf()

    fun main(vararg args: String) {
        debug = "-d" in args
        val input = generateSequence { readLine()?.trimEnd() }.toList()

        debug { input }

        val interestingKeys = setOf("8", "42", "31", "11", "42 31", "42 11 31")
        (1..2).forEach {
            runPart(it, input)
            matches
                .filterKeys { id -> id in interestingKeys }
                .forEach { (id, strings) -> println("$id: $strings") }
        }
    }

    fun runPart(part: Int, input: List<String>) {
        val unparsedRules: Map<ID, String> = input.filter { ":" in it }
            .associate { it.substringBefore(":") to it.substringAfter(": ") }
            .toMutableMap()
            .apply {
                if (part == 2) {
                    put("8", "42 | 42 8")
                    put("11", "42 31 | 42 11 31")
                }
            }

        val rules: Map<ID, Rule> = unparsedRules
            .mapValues { (id, ruleStr) ->
                Rule(id).apply {
                    when {
                        '"' in ruleStr -> literal(ruleStr[1])
                        '|' in ruleStr -> union(ruleStr.split(" | "))
                        else -> concat(ruleStr.split(' '))
                    }
                }
            }.also { debug { it } }

        fun String.matches(): Boolean {
            val matcher = NFAFactory(rules).instantiate("0")
            forEach {
                matcher.step(it)
            }
            val match = matcher.state == ACCEPTED
            return match.also { if (it) debug { "'$this' matches" } }
        }

        val messages = input.filter { ":" !in it && it.isNotEmpty() }.sorted()
        debug { messages }

        val matchCount = messages.count { it.matches() }

        println("Part $part: $matchCount")
    }

    enum class State { START, INTERMEDIATE, ACCEPTED, REJECTED }

    interface NFA {
        val ruleId: ID
        val state: State
        val terminated: Boolean
        fun step(input: Char)
    }

    private class RealNFA(
        override val ruleId: ID,
        private val epsilons: MutableSet<NFA> = mutableSetOf(),
        private val subs: MutableList<NFA> = mutableListOf(),
        private val literals: MutableSet<Char> = mutableSetOf(),
    ) : NFA {
        override var state: State = START
            private set
        override var terminated: Boolean = false
            private set

        private val string = StringBuilder()

        override fun step(input: Char) {
            string.append(input)
            if (terminated) return reject()

            state = INTERMEDIATE

            stepLiterals(input)
            stepEpsilons(input)
            stepSubs(input)

            if (noMoreTransitions()) {
                terminated = true
                if (state != ACCEPTED) state = REJECTED
            }
            updateCache()
        }

        private fun stepLiterals(input: Char) {
            if (literals.isEmpty()) return
            state = if (input in literals) ACCEPTED else REJECTED
            literals.remove(input)
        }

        private fun stepEpsilons(input: Char) {
            epsilons.forEach { it.step(input) }
            if (epsilons.any { it.state == ACCEPTED }) state = ACCEPTED
        }

        private fun stepSubs(input: Char) {
            if (subs.isEmpty()) return

            subs.firstOrNull { it.state != ACCEPTED }?.step(input) ?: run { subs.lastOrNull()?.step(input) }

            if (subs.any { it.state == REJECTED }) {
                subs.clear()
            } else if (subs.all { it.state == ACCEPTED }) {
                state = ACCEPTED
            }
        }

        fun matchChunks(string: String, matchRuleID: ID, startFrom: Int): Sequence<Int> =
            sequence { // returns number of chunks
                var advance = 1
                while (startFrom + advance < string.length) {
                    if (matches[matchRuleID]!!.contains(string.substring(startFrom until startFrom + advance))) {

                    }
                }
            }

        private fun updateCache() {
            if (state == ACCEPTED) {
                matches.computeIfAbsent(ruleId) { mutableSetOf() }.add(string.toString())
            }
        }

        private fun noMoreTransitions() = literals.isEmpty()
                && epsilons.all { it.terminated }
                && subs.all { terminated }

        private fun reject() {
            state = REJECTED
        }
    }

    class NFAFactory(val spec: Map<ID, Rule>) {
        fun instantiate(id: ID): NFA = if (' ' in id) instantiateConcatenation(id) else ProxyNFA(id)

        private fun instantiateConcatenation(id: ID) = RealNFA(
            ruleId = id,
            subs = id.split(' ').map { instantiate(it) }.toMutableList(),
        )

        private fun instantiateReal(id: ID) = instantiateReal(spec[id]!!)

        private fun instantiateReal(rule: Rule): NFA = RealNFA(
            ruleId = rule.id,
            epsilons = rule.epsilons.map { instantiate(it) }.toMutableSet(),
            subs = rule.subs.map { instantiate(it) }.toMutableList(),
            literals = rule.literals.toMutableSet(),
        )

        private inner class ProxyNFA(override val ruleId: ID) : NFA {
            private val delegate = lazy { instantiateReal(ruleId) }
            override val state: State get() = delegate.value.state
            override val terminated: Boolean get() = delegate.value.terminated

            override fun step(input: Char) = Unit.also { delegate.value.step(input) }
        }
    }

    class Rule(val id: ID) {
        val epsilons: MutableSet<ID> = mutableSetOf()
        val subs: MutableList<ID> = mutableListOf()
        val literals: MutableSet<Char> = mutableSetOf()

        fun literal(char: Char) = apply { literals.add(char) }
        fun concat(ids: Iterable<ID>) = apply { ids.forEach { subs.add(it) } }
        fun union(ids: Iterable<ID>) = apply { ids.forEach { epsilons.add(it) } }

        override fun toString(): String = "Rule $id"
    }
}
