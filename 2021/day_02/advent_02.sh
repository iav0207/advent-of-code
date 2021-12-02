#!/bin/bash

v=0 h=0
while read line
do
    direc="${line:0:1}"
    d="${line##*\ }"
    case $direc in
        'u') ((v=v-d)) ;;
        'd') ((v=v+d)) ;;
        *) ((h=h+d)) ;;
    esac
done
echo "${v}*${h}" | bc

