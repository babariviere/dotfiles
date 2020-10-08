source $HOME/.config/nvim/general/settings.vim
source $HOME/.config/nvim/general/plugins.vim
source $HOME/.config/nvim/general/neovide.vim
" source $HOME/.config/nvim/themes/one.vim
source $HOME/.config/nvim/themes/palenight.vim
" source $HOME/.config/nvim/themes/ayu.vim
source $HOME/.config/nvim/keys/mappings.vim
source $HOME/.config/nvim/keys/which-key.vim
" source $HOME/.config/nvim/plugins/coc.vim
source $HOME/.config/nvim/plugins/codi.vim
source $HOME/.config/nvim/plugins/goyo.vim
source $HOME/.config/nvim/plugins/quickscope.vim
source $HOME/.config/nvim/plugins/neoformat.vim
source $HOME/.config/nvim/plugins/rainbow.vim
" source $HOME/.config/nvim/plugins/rnvimr.vim
source $HOME/.config/nvim/plugins/signify.vim
" source $HOME/.config/nvim/plugins/startify.vim
source $HOME/.config/nvim/plugins/asciidoctor.vim
source $HOME/.config/nvim/plugins/languagetool.vim

lua require 'plug-colorizer'
lua require 'plug-lsp'

" TODO: take a look at conceal (to have symbols)

" Reload `init.vim` on write
" au! BufWritePost $MYVIMRC source %

" TODO move me
function! LspStatus() abort
  if luaeval('#vim.lsp.buf_get_clients() > 0')
    return luaeval("require('lsp-status').status()")
  endif

  return ''
endfunction

set showtabline=2
let g:lightline = extend(g:lightline, {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ],
      \   'right': [ [ 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'fileformat', 'fileencoding', 'filetype', 'lspstatus' ] ]
      \ },
      \ 'tabline': {
      \   'left': [ [ 'buffers' ] ],
      \   'right': [ [] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'FugitiveHead',
      \   'lspstatus': 'LspStatus',
      \ },
      \ 'component_expand': {
      \   'buffers': 'lightline#bufferline#buffers'
      \ },
      \ 'component_type': {
      \   'buffers': 'tabsel'
      \ },
      \ 'tabline_separator': { 'left': '', 'right': '' },
      \ 'tabline_subseparator': { 'left': '', 'right': '' }
      \ })

" call airline#parts#define_function('lsp_status', 'LspStatus')
" call airline#parts#define_condition('lsp_status', 'luaeval("#vim.lsp.buf_get_clients() > 0")')


" TODO move me
autocmd InsertLeave,BufEnter,BufWinEnter,TabEnter,BufWritePost * :lua require'lsp_extensions'.inlay_hints{ prefix = '» ', highlight = "NonText" }

" Remove trailing spaces
function ShowSpaces(...)
  let @/='\v(\s+$)|( +\ze\t)'
  let oldhlsearch=&hlsearch
  if !a:0
    let &hlsearch=!&hlsearch
  else
    let &hlsearch=a:1
  end
  return oldhlsearch
endfunction

function TrimSpaces() range
  let oldhlsearch=ShowSpaces(1)
  execute a:firstline.",".a:lastline."substitute ///gec"
  let &hlsearch=oldhlsearch
endfunction

command -bar -nargs=? ShowSpaces call ShowSpaces(<args>)
command -bar -nargs=0 -range=% TrimSpaces <line1>,<line2>call TrimSpaces()

" TODO move me
" IndentLine {{
let g:indentLine_char = '│'
let g:indentLine_first_char = '│'
let g:indentLine_showFirstIndentLevel = 1
let g:indentLine_concealcursor = "nv"
let g:indentLine_setConceal = 2
" let g:indentLine_setColors = 0
" }}
