import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

debug = "-d" in args
val input = generateSequence { readLine()?.trimEnd() }.toList()

debug { input }

