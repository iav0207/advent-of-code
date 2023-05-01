map <F5> :!clear<CR>
map <F6> <F5>:!go run -- advent_{{{DAY}}}.go -d < example.txt<CR>
map <F7> <F5>:!go run -- advent_{{{DAY}}}.go < input.txt<CR>
map <F8> <F5>:!go run -- advent_{{{DAY}}}.go -d < input.txt<CR>

set noexpandtab

e advent_{{{DAY}}}.go
vne input.txt

