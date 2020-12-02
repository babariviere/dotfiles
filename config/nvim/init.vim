lua require 'baba.plugins'

source $HOME/.config/nvim/general/settings.vim
" source $HOME/.config/nvim/general/plugins.vim
source $HOME/.config/nvim/general/neovide.vim
" source $HOME/.config/nvim/themes/one.vim
" source $HOME/.config/nvim/themes/palenight.vim
" source $HOME/.config/nvim/themes/ayu.vim
source $HOME/.config/nvim/themes/dracula.vim
source $HOME/.config/nvim/keys/mappings.vim
source $HOME/.config/nvim/keys/which-key.vim
" source $HOME/.config/nvim/plugins/coc.vim
source $HOME/.config/nvim/plugins/codi.vim
source $HOME/.config/nvim/plugins/goyo.vim
source $HOME/.config/nvim/plugins/quickscope.vim
" source $HOME/.config/nvim/plugins/neoformat.vim
" source $HOME/.config/nvim/plugins/rainbow.vim
" source $HOME/.config/nvim/plugins/rnvimr.vim
source $HOME/.config/nvim/plugins/signify.vim
" source $HOME/.config/nvim/plugins/startify.vim
source $HOME/.config/nvim/plugins/asciidoctor.vim
source $HOME/.config/nvim/plugins/languagetool.vim
source $HOME/.config/nvim/plugins/snippets.vim

lua require 'baba.colorizer'
lua require 'baba.lsp'
lua require 'baba.formatter'
lua require 'baba.statusline'
lua require 'baba.treesitter'

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


" Remove trailing spaces
function! s:trim_trailing_whitespace() abort
  let l:view = winsaveview()
  keeppatterns %substitute/\m\s\+$//e
  call winrestview(l:view)
endfunction

augroup trim_spaces
  autocmd!
  autocmd BufWritePre * call <SID>trim_trailing_whitespace()
augroup END

let g:rooter_patterns = ['.git', '_darcs', '.hg', '.bzr', '.svn', 'Makefile', 'Cargo.toml', 'mix.exs']
let g:rooter_silent_chdir = 1

let g:AutoPairsMultilineClose = 0

" TODO: neuron is spawned for the first file open so disable it for now.
" let g:neuron_dir = $HOME.'/notes/'

lua require'surround'.setup{}
lua require'bufferline'.setup()

" Flow configuration
lua << EOF
function flow_after_switch(path)
  vim.api.nvim_command('edit '..path)
  require'telescope.builtin'.find_files{}
end
EOF

nnoremap <silent> <leader>pp :lua require('flow').switch_project(flow_after_switch)<CR>
nnoremap <silent> <leader>pc :lua require('flow').clone_project(flow_after_switch)<CR>

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
