" set leader key
let g:mapleader = "\<Space>"

syntax enable
set hidden									            " Keep hidden buffers open
set nowrap									            " Disables line wrapping
set encoding=utf-8					            " Encoding displayed
set pumheight=10						            " Makes popup menu smaller
set fileencoding=utf-8			            " The encoding written to file
"set cmdheight=2							            " Have more space to see command messages
set splitbelow							            " Horizontal splits will automatically be below
set splitright							            " Vertical splits will automatically be to the right
set t_Co=256								            "	Support 256 colors
set tabstop=2								            " Insert 2 spaces for a tab
set softtabstop=2						            " Insert 2 spaces when using tab (insert spaces and not a tab)
set shiftwidth=2						            " The number of space characters inserted for indentation
set smarttab								            " Makes tabbing smarter (realize you have 2 or 4 spaces)
set expandtab                           " Converts tabs to spaces
set smartindent                         " Makes indenting smart
set autoindent                          " Good auto indent
set number                              " Line numbers
set cursorline                          " Enable highlighting of the current line
set showtabline=2                       " Always show tabs
set nobackup                            " This is recommended by coc
set nowritebackup                       " This is recommended by coc
set updatetime=200                      " Faster completion
set timeoutlen=500                      " By default timeoutlen is 1000 ms
set formatoptions-=cro                  " Stop newline continution of comments
set clipboard=unnamedplus               " Copy paste between vim and everything else
set inccommand=nosplit
set smartcase                           " Smart case search
set title                               " Set window title
set list
set listchars=tab:»\ ,extends:›,precedes:‹,nbsp:·,trail:·

set termguicolors
filetype plugin indent on

" check for and load file changes
autocmd WinEnter,BufWinEnter,FocusGained * checktime

" disable swapfile to avoid errors on load
set noswapfile

set guifont="JetBrains Mono Nerd Font:h12"

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = ''
let g:airline_powerline_fonts = 0

scriptencoding utf-8
