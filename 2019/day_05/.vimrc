map <F5> :!clear<CR>
map <F6> <F5>:!go run -- advent_05.go -d < example.txt<CR>
map <F7> <F5>:!go run -- advent_05.go < input.txt<CR>
map <F8> <F5>:!go run -- advent_05.go -d < input.txt<CR>

set noexpandtab

e advent_05.go
vne input.txt

