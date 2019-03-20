let use_coc = 1
let use_deoplete = 0

for f in split(glob('~/.config/nvim/config/*.vim'), '\n')
    exe 'source' f
endfor

call plug#begin('~/.local/share/nvim/plugged')

Plug 'roxma/nvim-yarp'

Plug 'morhetz/gruvbox'
Plug 'ayu-theme/ayu-vim'
Plug 'drewtempelmeyer/palenight.vim'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'rust-lang/rust.vim'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'hail2u/vim-css3-syntax'
Plug 'sheerun/vim-polyglot'

Plug 'rhysd/vim-clang-format'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'Shougo/neco-syntax'
Plug 'Shougo/neco-vim'

Plug 'w0rp/ale'

Plug 'hashivim/vim-terraform'
Plug 'juliosueiras/vim-terraform-completion'

if use_coc
    call CocPlug()
endif

if use_deoplete
    call DeopletePlug()
endif

Plug 'airblade/vim-gitgutter'

Plug 'farmergreg/vim-lastplace'

Plug 'junegunn/goyo.vim'

Plug 'pest-parser/pest.vim'
Plug '~/projects/reiu/vim'

call plug#end()

set number
set relativenumber
set tabstop=4
set softtabstop=0
set shiftwidth=4
set expandtab
set list lcs=trail:·,tab:»·

set previewheight=5

set termguicolors
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
set background=dark
let ayucolor="mirage"
colorscheme palenight

" Transparent background
"highlight Normal ctermbg=none
"highlight NonText ctermbg=none
"highlight Normal guibg=none
"highlight NonText guibg=none

"let g:python_host_prog  = '/usr/bin/python2.7'
"let g:python3_host_prog = '/usr/bin/python3.6'
set hidden

" Force sign column
set signcolumn=yes

" Completion settings
" set completeopt=noinsert,menuone,noselect
" set shortmess+=c
set noshowmode

" Wildmenu
set wildoptions=pum
set pumblend=20

inoremap <silent><expr> <C-j>  pumvisible() ? "\<C-n>" : "\<C-j>"
inoremap <silent><expr> <C-k>  pumvisible() ? "\<C-p>" : "\<C-k>"
inoremap <silent><expr> <Up>   pumvisible() ? "\<Esc>ka" : "\<Up>"
inoremap <silent><expr> <Down> pumvisible() ? "\<Esc>ja" : "\<Down>"
" inoremap <silent><expr> <Tab> pumvisible() ? "\<C-y>" : "\<Tab>"


" Go
let g:go_fmt_command = "goimports"
let g:go_auto_type_info = 1
let g:go_addtags_transform = "snakecase"

" Show completion popup on Ctrl Space
inoremap <expr> <C-Space> (pumvisible() ? (col('.') > 1 ? '<Esc>i<Right>' : '<Esc>i') : '') .
            \ '<C-x><C-o><C-r>=pumvisible() ? "\<lt>C-n>\<lt>C-p>" : ""<CR>'

" Ocaml
"let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
"execute "set rtp+=" . g:opamshare . "/merlin/vim"

" Airline
let g:airline_theme = "deus"

" Fzf
let g:fzf_buffers_jump = 1
nnoremap <silent> <C-p> :Files<CR>

" Rust
let g:rustfmt_autosave = 1

" vim illuminate
hi illuminatedWord cterm=none guibg=#404040

" ALE
let g:ale_enabled = 1
let g:ale_linters = {}
let g:ale_linters.go = ['gometalinter']
let g:ale_linters.fish = []
let g:ale_linters.cpp = []
let g:ale_linters.kotlin = []
let g:ale_completion_enabled = 0
let g:ale_set_loclist = 0
let g:ale_keep_list_window_open = 0
let g:ale_virtualtext_cursor = 1

" Clang-format
let g:clang_format#code_style = "llvm"
let g:clang_format#auto_format = 1
let g:clang_format#detect_style_file = 1

autocmd BufNewFile,BufRead *.jsschema set ft=json

" Spell check
" set spelllang=en_us
" set spell!

let mapleader = ","

map <leader>c :!compile <c-r>%<CR>
map <leader>o :call jobstart("open ".expand("%"))<CR>

" groff files
autocmd BufRead,BufNewFile *.ms set filetype=groff

map <leader>sf :setlocal spell spelllang=fr<CR>

let g:tex_comment_nospell=1

" goyo
nmap <tab> :Goyo<CR>

" asm
au FileType asm set ft=nasm
let g:ale_nasm_nasm_options='-f elf64'

call CocSetup(use_coc)
call DeopleteSetup(use_deoplete)

" disable gitgutter for now
let g:gitgutter_enabled = 0

" Terraform
let g:terraform_align=1
let g:terraform_fmt_on_save=1
