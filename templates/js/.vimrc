map <F5> :!clear<CR>
map <F6> <F5>:w<CR>
map <F7> <F6>:!./advent_{{{DAY}}}.js < input.txt<CR>
map <F8> <F6>:!./advent_{{{DAY}}}.js -d < example.txt<CR>

e advent_{{{DAY}}}.js
vne input.txt

