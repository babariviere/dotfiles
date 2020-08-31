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
let g:which_key_map[';'] = [ ':Commands'                            , 'commands' ]
" let g:which_key_map['e'] = [ ':e <C-R>=expand("%:p:h") . "/" <CR>'  , 'explorer' ]
let g:which_key_map['h'] = [ '<C-W>s'                               , 'split below']
let g:which_key_map['r'] = [ ':RnvimrToggl'                         , 'ranger' ]
let g:which_key_map['S'] = [ ':Startify'                            , 'start screen' ]
let g:which_key_map['T'] = [ ':Rg'                                  , 'search text' ]
let g:which_key_map['v'] = [ '<C-W>v'                               , 'split right']
let g:which_key_map['z'] = [ 'Goyo'                                 , 'zen' ]
let g:which_key_map[' '] = [ ':Files'                               , 'search files']
let g:which_key_map[','] = [ ':Buffers'                             , 'open buffers']

" Edit file in current file's directory
nnoremap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
let g:which_key_map.e = 'edit in directory'

" s is for search
let g:which_key_map.s = {
      \ 'name' : '+search' ,
      \ '/' : [':History/'     , 'history'],
      \ ';' : [':Commands'     , 'commands'],
      \ 'a' : [':Ag'           , 'text Ag'],
      \ 'b' : [':BLines'       , 'current buffer'],
      \ 'B' : [':Buffers'      , 'open buffers'],
      \ 'c' : [':Commits'      , 'commits'],
      \ 'C' : [':BCommits'     , 'buffer commits'],
      \ 'f' : [':Files'        , 'files'],
      \ 'g' : [':GFiles'       , 'git files'],
      \ 'G' : [':GFiles?'      , 'modified git files'],
      \ 'h' : [':History'      , 'file history'],
      \ 'H' : [':History:'     , 'command history'],
      \ 'm' : [':Marks'        , 'marks'] ,
      \ 'M' : [':Maps'         , 'normal maps'] ,
      \ 'p' : [':Rg'           , 'project lines'] ,
      \ 'P' : [':Tags'         , 'project tags'],
      \ 's' : [':Snippets'     , 'snippets'],
      \ 'S' : [':Colors'       , 'color schemes'],
      \ 'T' : [':BTags'        , 'buffer tags'],
      \ 'w' : [':Windows'      , 'search windows'],
      \ 'y' : [':Filetypes'    , 'file types'],
      \ }

" Register which key map
call which_key#register('<Space>', "g:which_key_map")
