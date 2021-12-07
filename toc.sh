#!/bin/bash

srcs=$(find . -iname 'advent*.*')
function langof() {
    case "${1##*\.}" in
            'F')    echo Fortran  ;;
            'cpp')  echo C++      ;;
            'go')   echo Go       ;;
            'pl')   echo Perl     ;;
            'py')   echo Python   ;;
            'sh')   echo Bash     ;;
    esac
}

for year in $(echo 20*); do
    echo "- [$year](https://adventofcode.com/$year)"
    for folder in $(ls $year | grep day); do
        day="${folder##*\_}"
        path="./${year}/day_${day}"
        langs=$(ls $path| while read src; do echo $(langof $src); done | sort -u | tr '\n' ' ')
        echo "  + [Day ${day}](${path}) $langs"
    done
done

