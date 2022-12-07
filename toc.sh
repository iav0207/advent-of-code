#!/bin/bash

function langof() {
    case "${1##*\.}" in
            'F')          echo Fortran          ;;
            'c')          echo C                ;;
            'coffee')     echo CoffeeScript     ;;
            'cpp')        echo C++              ;;
            'cs')         echo 'C#'             ;;
            'go')         echo Go               ;;
            'groovy')     echo Groovy           ;;
            'java')       echo Java             ;;
            'jl')         echo Julia            ;;
            'js')         echo JavaScript       ;;
            'kt')         echo Kotlin           ;;
            'kts')        echo KotlinScript     ;;
            'pl')         echo Perl             ;;
            'py')         echo Python           ;;
            'r')          echo R                ;;
            'rb')         echo Ruby             ;;
            'rs')         echo Rust             ;;
            'scala')      echo Scala            ;;
            'sh')         echo Bash             ;;
            'ts')         echo TypeScript       ;;
            'vb')         echo Visual Basic     ;;
    esac
}

printf "# Index\n\n"

for year in $(echo 20* | sort); do echo "- [${year}](#${year})"; done

for year in $(echo 20* | sort); do
    printf "\n## ${year}\n\nhttps://adventofcode.com/$year\n\n[Jump to top](#index)\n\n"
    min_day=$(ls $year | grep day_ | cut -d'_' -f2- | head -n1)
    max_day=$(ls $year | grep day_ | cut -d'_' -f2- | tail -n1)
    for day in $(seq -f '%02g' $min_day $max_day); do
        path="./${year}/day_${day}"
        [ -d "${path}" ] || echo "  + Day ${day} Not done"
        [ -d "${path}" ] || continue
        langs=$(ls $path| while read src; do echo $(langof $src); done | sort -u | tr '\n' ' ')
        desc=$(grep -r '^Desc ' $path | cut -d':' -f2- | cut -d' ' -f2-)
        echo "- [Day ${day}](${path}) $langs"
        [ -z "${desc}" ] || echo "  + ${desc}"
    done
done

