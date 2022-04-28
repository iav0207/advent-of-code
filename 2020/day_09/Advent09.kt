package advent2020.day09

var debug = false
fun debug(a: Any) = if (debug) println(a) else Unit
fun debug(a: () -> Any) = if (debug) println(a()) else Unit

const val window = 25

fun main(vararg args: String) {
    debug = "-d" in args

    val input: List<Long> = generateSequence { readLine()?.trimEnd()?.toLong() }.toList()
    
    val firstInvalid = input.findFirstInvalid().also {
        println("The first invalid number is $it")
    }
    input.findMinMaxOfContSet(firstInvalid).apply {
        println("The min and max of the contiguous set summing up to $firstInvalid is ($first, $second)")
        println("Their sum is ${first + second}")
    }
}

fun List<Long>.findFirstInvalid(): Long {
    val buffer = ArrayDeque<Long>(take(window))
    for (next in slice(window until size)) {
        if (!buffer.isValid(next)) return next
        buffer.removeFirst()
        buffer.addLast(next)
    }
    error("Not found")
}

fun ArrayDeque<Long>.isValid(next: Long): Boolean {
    debug { this }
    for (i in 0 until size - 1) {
        for (j in i + 1 until size) {
            if (get(i) + get(j) == next) return true.also { debug { "${get(i)} + ${get(j)} == $next" } }
            debug { "${get(i)} + ${get(j)} != $next" }
        }
    }
    return false
}

fun List<Long>.findMinMaxOfContSet(num: Long): Pair<Long, Long> {
    for (i in 0 until size - 1) {
        var sum = get(i)
        for (j in i + 1 until size) {
            sum += get(j)
            if (sum > num) break
            if (sum == num) return slice(i..j).findMinMax()
        }
    }
    error("Not found")
}

fun List<Long>.findMinMax(): Pair<Long, Long> = minOrNull()!! to maxOrNull()!!

