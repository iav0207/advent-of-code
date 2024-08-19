cd "$(dirname "$0")"
jarfile="$(mktemp -d)/f.jar"
kotlinc Advent01.kt -include-runtime -d "${jarfile}"
kotlin -cp "${jarfile}" advent2022.day01.Advent01Kt < input.txt

