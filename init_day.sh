#!/bin/bash

function require() {
    name=$1 val=$2
    if [[ -z "$val" ]]; then
        echo "Parameter $name is required" 1>&2
        exit 1
    fi
}

function replace_content() {
    folder=$1 to_replace=$2 replace_with=$3
    sed -i '' -e "s/${to_replace}/${replace_with}/g" $(find $folder -type f)
}

function rename_files() {
    folder=$1 to_replace=$2 replace_with=$3
    for orig in $(find $folder -type f -iname "*${to_replace}*"); do
        repl="${orig//$to_replace/$replace_with}"
        mv $orig $repl
    done
}

function use_template() {
    template_dir=$1 destination=$2
    cp -R $template_dir $destination
    replace_content $destination '{{{DAY}}}'    "${day_padded}"
    replace_content $destination '{{{YEAR}}}'   "${year}"
    rename_files    $destination 'DAY'          "${day_padded}"
}

function init_day() {
    year=$1 day=$2 lang=${3:-kotlin}
    require year $year
    require day $day
    day_padded="$(printf '%02d' "${day}")"
    cd "$(dirname $0)"
    new_dir="${year}/day_${day_padded}"
    use_template "templates/${lang}" "${new_dir}"
    cd $new_dir
    open https://adventofcode.com/$year/day/$day/input
    open https://adventofcode.com/$year/day/$day
    vim                             \
        -c ':so .vimrc'             \
        -c ':vne input.txt'         \
        "Advent${day_padded}.kt"
}

set -e
init_day $@

