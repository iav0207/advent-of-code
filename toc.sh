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

for year in $(echo 20*); do
    echo "- [$year](https://adventofcode.com/$year)"
    for folder in $(ls $year | grep day); do
        day="${folder##*\_}"
        path="./${year}/day_${day}"
        langs=$(ls $path| while read src; do echo $(langof $src); done | sort -u | tr '\n' ' ')
        desc=$(grep -r '^Desc ' $path | cut -d':' -f2- | cut -d' ' -f2-)
        echo "  + [Day ${day}](${path}) $langs"
        [ -z "${desc}" ] || echo "    * ${desc}"
    done
done

