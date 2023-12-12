package advent2023.day12

import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit
fun <T : Any> T.debug(a: (T) -> Any = { this }): T = also { if (debug) println(a(it)) }

fun main(vararg args: String) {
    debug = "-d" in args
    val records: List<Pair<DamagedRecord, Signature>> = generateSequence { readlnOrNull()?.trimEnd() }.toList()
        .map { it.split(" ") }
        .map { DamagedRecord(it[0]) to it[1].split(",").map { n -> n.toInt() } }

    records.asSequence().sumOf { (rec, sig) ->
        onDot(rec.groups(), sig)
            .debug { "${rec.code} :: $sig - $it arrangements" }
    }.also { println("Part 1: $it") }
}

data class DamagedRecord(val code: String)
typealias Signature = List<Int>
data class Group(val c: Char, val n: Int)

fun DamagedRecord.groups(): List<Group> = mutableListOf<Group>().apply {
    var n = 0
    var prev = '\n'
    for (c in code) {
        if (c == prev) {
            n++
            continue
        }
        if (n > 0) add(Group(prev, n))
        prev = c
        n = 1
    }
    if (n > 0) add(Group(prev, n))
}.debug { "groups($code) = $it" }

fun onDot(groups: List<Group>, sign: Signature): Long = when {
    sign.isOverConsumed() -> 0L
    groups.isEmpty() && sign.isFullyConsumed() -> 1L
    groups.isEmpty() -> 0L
    groups.first().c == '.' -> onDot(groups.drop(1), sign)
    sign.isFullyConsumed() -> 0L
    groups.first().c == '#' -> onHash(groups.drop(1), sign.consumeHashes(groups.first().n))
    // '?'
    else -> groups.consumeOneChar().let { nextGroups ->
        val nextSign = sign.consumeOne()
        onDot(nextGroups, nextSign) + onHash(groups, sign)
    }
}.debug { "onDot $groups $sign = $it" }

fun onHash(groups: List<Group>, sign: Signature): Long = when {
    sign.isOverConsumed() -> 0L
    groups.isEmpty() && sign.isFullyConsumed() -> 1L
    groups.isEmpty() -> 0L
    sign.isFullyConsumed() -> 0L
    groups.takeChars(sign.first()).any { it.c == '.' } -> 0L
    groups.charAt(sign.first() + 1) == '#' -> 0L
    groups.first().c == '#' -> when {
        groups.first().n < sign.first() -> onDot(groups.consumeChars(sign.first()), sign.consumeHashes(sign.first()))
        groups.first().n == sign.first() -> onDot(groups.drop(1), sign.drop(1)) // equivalent to the previous branch
        else -> 0L
    }
    else -> onDot(groups.consumeChars(sign.first()), sign.drop(1)).debug { "onHash else" }
}.debug { "onHash $groups $sign = $it" }

fun List<Group>.charAt(i: Int): Char? = takeChars(i).lastOrNull()?.c
fun List<Group>.takeChars(count: Int): Sequence<Group> {
    var remaining = count
    return asSequence().onEach { remaining -= it.n }.takeWhile { remaining >= 0 }
}
fun List<Group>.consumeChars(count: Int): List<Group> {
    var remaining = count
    return mapNotNull { g ->
        val dec = minOf(remaining, g.n)
        remaining -= dec
        Group(g.c, g.n - dec).takeIf { g.n > 0 }
    }
}
fun List<Group>.consumeOneChar() = consumeChars(1)
    .debug { "$this.consumeOneChar() = $it" }

fun Signature.consumeOne() = consumeHashes(1).filter { it > 0 }
fun Signature.isFullyConsumed() = all { it == 0 }
fun Signature.isOverConsumed() = firstOrNull()?.let { it < 0 } ?: false
fun Signature.consumeHashes(count: Int) = List(size) { i -> if (i == 0) first() - count else get(i) }.filter { it == 0 }

fun DamagedRecord.arrangements(sign: Signature): Sequence<String> = arrangements(sign, "", 0)

fun DamagedRecord.arrangements(sign:Signature, s: String, i: Int): Sequence<String> = when {
    i == code.length -> sequenceOf(s)
    code[i] == '?' -> sequenceOf('.', '#').flatMap { arrangements(sign, s + it, i + 1) }
    else -> arrangements(sign, s + code[i], i + 1)
}

fun String.sign(): Signature = "#+".toRegex().findAll(this)
    .map { it.value.length }
    .toList()
    .debug { "sign($this) = $it" }

