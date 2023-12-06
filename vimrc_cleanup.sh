#!/bin/bash

for lang in Kotlin Python Go; do
    paths=$(awk -F'[()]' -v find="${lang}\$" '$0 ~ find { print $2 }' index.md)
    for path in $paths; do
        vimrc=$path/.vimrc
        if [ -f $vimrc ]; then rm $vimrc; echo cleaned up $vimrc; fi
    done
done
