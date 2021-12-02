#!/bin/bash

v=0 h=0 aim=0
while read line
do
    direc="${line:0:1}"
    d="${line##*\ }"
    case $direc in
        'u') ((aim=aim-d)) ;;
        'd') ((aim=aim+d)) ;;
        'f') ((h=h+d)); ((v=v+(aim*d))) ;;
    esac
done
echo "${v}*${h}" | bc

