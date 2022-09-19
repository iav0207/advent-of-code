map <F5> :!clear<CR>
map <F7> <F6>:!./advent_{{{DAY}}}.py -d < example.txt<CR>
map <F7> <F6>:!./advent_{{{DAY}}}.py < input.txt<CR>
map <F8> <F6>:!./advent_{{{DAY}}}.py -d < input.txt<CR>

e advent_{{{DAY}}}.py
vne input.txt

