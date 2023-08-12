map <F5> :!clear<CR>
map <F6> <F5>:!go run -- advent_04.go -d < example.txt<CR>
map <F7> <F5>:!go run -- advent_04.go < input.txt<CR>
map <F8> <F5>:!go run -- advent_04.go -d < input.txt<CR>

set noexpandtab

e advent_04.go
vne input.txt

