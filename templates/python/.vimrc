map <F5> :!clear<CR>:w<CR>
map <F6> <F5>:!./advent_{{{DAY}}}.py -d < example.txt<CR>
map <F7> <F5>:!./advent_{{{DAY}}}.py < input.txt<CR>
map <F8> <F5>:!./advent_{{{DAY}}}.py -d < input.txt<CR>

e advent_{{{DAY}}}.py
vne input.txt

