import kotlin.math.*

var debug = false
fun debug(a: () -> Any): Unit = if (debug) println(a()) else Unit

debug = "-d" in args
val input = generateSequence { readLine()?.trimEnd() }.toList()

val alphabet = CharRange('a', 'z').toList() + CharRange('A', 'Z').toList()
val resultItems = mutableListOf<Char>()

fun String.compartments(): List<Set<Char>> = listOf(
    substring(0, length / 2).toSet(),
    substring(length / 2, length).toSet(),
)

fun String.commonItems(): Set<Char> = compartments().run { get(0).intersect(get(1)) }

for (line in input) {
    resultItems.addAll(line.commonItems())
}

debug { resultItems }

fun Char.prio() = alphabet.indexOf(this) + 1

println("Part 1: ${resultItems.sumOf { it.prio() }}")

typealias Group = List<String>
fun List<String>.groups(): List<Group> = chunked(3)

fun Group.badge() = map { it.toSet() }
    .fold(alphabet.toSet()) { acc, it -> acc.intersect(it) }
    .first()

val badges = input.groups().map { it.badge() }
debug { badges }

println("Part 1: ${badges.sumOf { it.prio() }}")

