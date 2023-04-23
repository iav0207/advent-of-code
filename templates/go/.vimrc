map <F5> :!clear<CR>
map <F6> <F5>:!go run -- advent_{{{DAY}}} -d < example.txt<CR>
map <F7> <F5>:!go run -- advent_{{{DAY}}} < input.txt<CR>
map <F8> <F5>:!go run -- advent_{{{DAY}}} -d < input.txt<CR>

set noexpandtab

e advent_{{{DAY}}}.go
vne input.txt

