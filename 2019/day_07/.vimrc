map <F5> :!clear<CR>
map <F6> <F5>:!go run -- advent_07.go -d < example.txt<CR>
map <F7> <F5>:!go run -- advent_07.go < input.txt<CR>
map <F8> <F5>:!go run -- advent_07.go -d < input.txt<CR>

set noexpandtab

e advent_07.go
vne input.txt
