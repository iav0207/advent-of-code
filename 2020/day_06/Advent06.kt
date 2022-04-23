package advent2020.day06

fun main() {
    val groups: List<Group> = generateSequence { readlnOrNull() }
        .joinToString("\n")
        .split("\n\n")
        .map { it.split("\n") }

    val anyYesSum = groups.sumOf { it.countAnyYesQuestions() }
    val allYesSum = groups.sumOf { it.countAllYesQuestions() }
    println("The sum of group counts is: any=$anyYesSum all=$allYesSum")
}

typealias Group = List<String>

fun Group.countAnyYesQuestions(): Int = flatMap { it.toSet() }.toSet().size
fun Group.countAllYesQuestions(): Int = map { it.toSet() }.reduce { acc, it -> acc.intersect(it) }.size

