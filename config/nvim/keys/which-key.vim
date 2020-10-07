" Map leader to which_key
nnoremap <silent> <leader> :silent WhichKey '<Space>'<CR>
vnoremap <silent> <leader> :silent <c-u> :silent WhichKeyVisual '<Space>'<CR>

" Create map to add keys to
let g:which_key_map =  {}
" Define a separator
let g:which_key_sep = '→'
" set timeoutlen=100

" Not a fan of floating windows for this
let g:which_key_use_floating_win = 0

" Change the colors if you want
highlight default link WhichKey          Operator
highlight default link WhichKeySeperator DiffAdded
highlight default link WhichKeyGroup     Identifier
highlight default link WhichKeyDesc      Function

" Hide status line
autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 noshowmode ruler

" Single mappings
let g:which_key_map['/'] = [ ':Commentary'                          , 'comment' ]
let g:which_key_map['h'] = [ '<C-W>s'                               , 'split below']
let g:which_key_map['S'] = [ ':Startify'                            , 'start screen' ]
let g:which_key_map['v'] = [ '<C-W>v'                               , 'split right']
let g:which_key_map['z'] = [ 'Goyo'                                 , 'zen' ]

" Edit file in current file's directory
nnoremap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
let g:which_key_map.e = 'edit in directory'

nnoremap <leader><space> <cmd>lua require'telescope.builtin'.find_files{}<CR>
let g:which_key_map[' '] = 'search files'

nnoremap <leader>, <cmd>lua require'telescope.builtin'.buffers{}<CR>
let g:which_key_map[','] = 'open buffers'


"" Buffers
nnoremap <leader>bd :bdelete<CR>

let g:which_key_map.b = {
      \ 'd': 'delete buffer'
      \ }

"" Search
nnoremap <leader>sb <cmd>lua require'telescope.builtin'.buffers{}<CR>
nnoremap <leader>sp <cmd>lua require'telescope.builtin'.live_grep{}<CR>

let g:which_key_map.s = {
      \ 'b': 'find buffers',
      \ 'p': 'find in project'
      \ }

" Register which key map
call which_key#register('<Space>', "g:which_key_map")
