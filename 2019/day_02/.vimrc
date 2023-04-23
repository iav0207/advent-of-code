map <F5> :!clear<CR>
map <F7> <F5>:!go run advent_02.go < input.txt<CR>
map <F8> <F5>:!go run -- advent_02.go -d < input.txt<CR>

set noexpandtab

e advent_02.go
vne input.txt

