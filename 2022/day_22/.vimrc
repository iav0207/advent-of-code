map <F5> :!clear<CR>:w<CR>
map <F6> <F5>:!./advent_22.py -d < example.txt<CR>
map <F7> <F5>:!./advent_22.py < input.txt<CR>
map <F8> <F5>:!./advent_22.py -d < input.txt<CR>

set ts=4
set expandtab

e advent_22.py
vne input.txt

