package advent2023.day20

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val input = generateSequence { readlnOrNull()?.trimEnd() }.toList()
    Circuit(input).apply {
        repeat(1000) { i -> debug { i }; pushTheButton() }
        println("Part 1: ${pulses[LO]!!.times(pulses[HI]!!)}")
    }
}

class Circuit(input: List<String>) {
    val pulses = mutableMapOf(LO to 0L, HI to 0L)

    private val modules: MutableMap<String, Module> = input
        .map { line -> line.split("( -> )|(, )".toRegex()) }
        .map {
            val ref = it.first()
            Module(
                name = ref.replace("[%&]".toRegex(), ""),
                type = ref.first(),
                outputs = it.drop(1),
            )
        }
        .associateBy { it.name }.toMutableMap()

    private val conStates: Map<ModuleName, MutableMap<ModuleName, Impulse>> = modules.values
        .flatMap { m -> m.outputs.map { it to m } }
        .filter { (receiver, _) ->
            modules.computeIfAbsent(receiver) { Module(receiver, receiver.first(), emptyList()) }.type == '&'
        }
        .groupBy({ it.first }, { it.second })
        .mapValues { (_, inputs) -> inputs.associate { it.name to LO }.toMutableMap() }
    private val flipStates: MutableMap<ModuleName, Boolean> = modules.values
        .filter { it.type == '%' }
        .associate { it.name to false }
        .toMutableMap()

    private fun inc(imp: Impulse, inc: Int = 1) = pulses.compute(imp) { _, count -> count!! + inc }

    fun pushTheButton() {
        val queue = ArrayDeque<Message>()
        queue.add(Message(LO, "button", "broadcaster"))
        inc(LO)

        while (queue.isNotEmpty()) {
            val (impulse, senderName, receiverName) = queue.removeFirst()
            val receiver = modules[receiverName]!!

            fun send(imp: Impulse) {
                receiver.outputs.forEach { queue.add(Message(imp, sender = receiverName, it).debug()) }
                inc(imp, receiver.outputs.size)
            }

            when (receiver.type) {
                '%' -> {
                    if (impulse == LO) flipStates.compute(receiverName) { _, state -> !state!! }!!.also { send(it) }
                }
                '&' -> {
                    val s = conStates[receiverName]!!
                    s[senderName] = impulse
                    send(if (s.values.all { it == HI }) LO else HI)
                }
                else -> send(impulse)
            }
        }
    }
}

typealias Type = Char
typealias ModuleName = String
data class Module(val name: String, val type: Type, val outputs: List<String>)
typealias Impulse = Boolean

data class Message(val impulse: Impulse, val sender: ModuleName, val receiver: ModuleName)

const val LO = false
const val HI = true
