#!/bin/bash

set -e
gfortran ADVENT_05.F -o advent_05
sed 's/\ -\>\ / /g' input.txt |
   sed 's/,/\ /g' |
   ./advent_05

