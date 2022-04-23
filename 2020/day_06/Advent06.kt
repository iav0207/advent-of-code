package advent2020.day06

fun main() {
    val groups: List<Group> = generateSequence { readGroup() }.toList()

    val anyYesSum = groups.sumOf { it.countAnyYesQuestions() }
    val allYesSum = groups.sumOf { it.countAllYesQuestions() }

    println("The sum of group counts is: any=$anyYesSum all=$allYesSum")
}

typealias Answers = Set<Char>
typealias Group = List<Answers>

fun readGroup(): Group? = generateSequence { readAnswersOrNull() }
    .toList()
    .takeUnless { it.isEmpty() }

fun readAnswersOrNull(): Answers? = readlnOrNull()
    ?.takeUnless { it.isEmpty() }
    ?.toSet()

fun Group.countAnyYesQuestions(): Int = flatMap { it }.toSet().size
fun Group.countAllYesQuestions(): Int = reduce { acc, it -> acc.intersect(it) }.size

