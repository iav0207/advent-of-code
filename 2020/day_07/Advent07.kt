package advent2020.day07

const val SHINY_GOLD = "shiny gold"

var debug = false
fun debug(a: Any) = if (debug) println(a) else Unit
fun debug(a: () -> Any) = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args

    val bags = generateSequence { readlnOrNull() }.map { it.parseBag() }.toList()

    val inverseIndex: Map<Color, List<Color>> = bags
        .flatMap { parent -> parent.children.map { child -> child.color to parent.color } }
        .groupBy { (child, _) -> child }
        .mapValues { (_, childParents) -> childParents.map { (_, parent) -> parent } }

    debug { inverseIndex.map { "${it.key} : ${it.value}" }.joinToString("\n") }
     
    val visited = mutableSetOf<Color>()

    fun dive(node: Color) {
        visited.add(node)
        val children = inverseIndex[node] ?: emptyList()
        for (next in children) if (next !in visited) dive(next)
    }

    dive(SHINY_GOLD)

    debug { visited }

    println("Found ${visited.size - 1} bags that contain at least one shiny gold bag")

    val colorToBag = bags.associateBy { it.color } 
    val sizes = mutableMapOf<Color, Int>()

    fun Bag.size(): Int = sizes.getOrPut(color) {
        children.sumOf { childSpec -> colorToBag[childSpec.color]!!.size() * childSpec.count } + 1 // 1 for 'self'
    }

    val shinyGoldBagSize = colorToBag[SHINY_GOLD]!!.size()
    println("One shiny gold bag contains ${shinyGoldBagSize - 1} bags") // -1 for 'self'
}

typealias Color = String

class ChildSpec(val color: Color, val count: Int) {
    override fun toString() = "$count $color bags"
}

class Bag(val color: Color, val children: List<ChildSpec> = emptyList()) {
    override fun toString() = "$color bag with up to ${children.joinToString(", ")}"
}

fun String.parseBag(): Bag {
    val (color, spec) = split(" bags contain ")
    if ("no other bags" in spec) return Bag(color)
    val children = spec.split(", ").map { it.parseChildSpec() }
    return Bag(color, children)
}

fun String.parseChildSpec(): ChildSpec = replace(childSpecEnding, "")
    .split(" ", limit = 2)
    .let { (countStr, color) -> ChildSpec(color, countStr.toInt()) }

val childSpecEnding = Regex(" bag[s]?[.]?")

