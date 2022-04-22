package advent2020.day04

import java.util.*
import kotlin.math.*

var debug = false
fun debug(a: Any) = if (debug) println(a) else Unit
fun debug(a: () -> Any) = if (debug) println(a()) else Unit

fun main(vararg args: String) {
    debug = "-d" in args

    val passports = generateSequence { readlnOrNull() }
        .joinToString(" ")
        .split("  ")
        .parse()

    val completeCount = passports.count { it.isComplete() }
    val validCount = passports.count { it.isValid() }
    println("$completeCount passports are complete")
    println("$validCount passports are valid")
}

typealias Key = String
typealias Value = String
typealias Passport = Map<Key, Value>

fun List<String>.parse(): List<Passport> = map { it.toPassport() }
fun String.toPassport(): Passport = split(" ").map { it.split(":") }.associate { (k, v) -> k to v }

fun Passport.isComplete(): Boolean = requiredFields.all { it in keys }

fun Passport.isValid(): Boolean = isComplete()
    && int("byr") in (1920 .. 2002)
    && int("iyr") in (2010 .. 2020)
    && int("eyr") in (2020 .. 2030)
    && str("hgt").isValidHeight().also { debug { "${str("hgt")} valid: $it" } }
    && str("hcl").matches(hclRegex).also { debug { "${str("hcl")} valid: $it" } }
    && str("ecl") in allowedEyeColors
    && str("pid").matches(pidRegex).also { debug { "${str("pid")} valid: $it" } }

fun Value.isValidHeight(): Boolean {
    val unit = slice(length - 2 until length)
    val value = slice(0 until length - 2).takeUnless { it.isEmpty() }?.toInt() ?: return false
    return when (unit) {
        "cm" -> value in (150 .. 193)
        "in" -> value in (59 .. 76)
        else -> false
    }
}

fun Passport.int(name: String) = get(name)!!.toInt()
fun Passport.str(name: String) = get(name)!!

val hclRegex = Regex("#[0-9a-f]{6}")
val pidRegex = Regex("\\d{9}")

val allowedEyeColors: Set<String> = setOf("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
val requiredFields: List<Key> = listOf(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid",
)

