package advent2020.day08

const val SHINY_GOLD = "shiny gold"

var debug = false
fun debug(a: Any) = if (debug) println(a) else Unit
fun debug(a: () -> Any) = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args

    val commands = generateSequence { readLine()?.trimEnd() }.toList()

    val original = Execution(commands)
    println("acc before looping = ${original.state.acc}")

    debug { "\naltering:" }

    val nopsAndJumps = original.history.filter { commands[it].run { "nop" in this || "jmp" in this } }

    for (candidate in nopsAndJumps) {
        val altered = original.withAltered(candidate)
        if (!altered.looped) {
            print("acc after the corrected program terminates = ${altered.state.acc}")
            break
        }
    }
}

class Execution(val program: List<String>) {
    val history: History = mutableSetOf()
    val state = StateMachine()

    val looped: Boolean = state.run {
        while (history.add(cur) && cur in program.indices) {
            debug { "${program[cur]} $this" }
            exec(program[cur])
        }
        cur in program.indices
    }

    fun withAltered(state: Int) = Execution(program.toMutableList().also { it[state] = it[state].flip() })
}

class StateMachine {
    var acc = 0
    var cur = 0

    fun exec(cmd: String) {
        when {
            "nop" in cmd -> cur++
            "acc" in cmd -> {
                cur++
                acc += cmd.operand()
            }
            "jmp" in cmd -> cur += cmd.operand()
            else -> error("Unknown command: $cmd")
        }
    }

    override fun toString() = "cur=$cur acc=$acc"
}

fun String.operand(): Int = split(" ")[1].toInt()

fun String.flip(): String = when {
    "nop" in this -> replace("nop", "jmp")
    "jmp" in this -> replace("jmp", "nop")
    else -> this
}

typealias History = MutableSet<Int>

