map <F5> :!clear<CR>
map <F7> <F5>:!./advent_{{{DAY}}}.js < input.txt<CR>
map <F8> <F5>:!./advent_{{{DAY}}}.js -d < example.txt<CR>

e advent_{{{DAY}}}.js
vne input.txt

