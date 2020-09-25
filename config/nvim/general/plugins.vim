" auto-install vim-plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  "autocmd VimEnter * PlugInstall
  "autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin(stdpath('data') . '/plugged')
  " Themes
  Plug 'ntk148v/vim-horizon'
  Plug 'rakr/vim-one'
  Plug 'ayu-theme/ayu-vim'
  Plug 'drewtempelmeyer/palenight.vim'

  " Status bar
  " Plug 'itchyny/lightline.vim'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'

  " Languages
  Plug 'sheerun/vim-polyglot'
  Plug 'LnL7/vim-nix'
  Plug 'habamax/vim-asciidoctor'

  " COC
  " Plug 'neoclide/coc.nvim', {'branch': 'release'}
  " Plug 'elixir-lsp/coc-elixir', {'do': 'yarn install && yarn prepack'}
  
  " Auto Pairs
  Plug 'jiangmiao/auto-pairs'

  " LSP nvim
  Plug 'neovim/nvim-lspconfig'
  Plug 'nvim-lua/completion-nvim'
  Plug 'nvim-lua/diagnostic-nvim'
  Plug 'nvim-lua/lsp-status.nvim'
  Plug 'tjdevries/lsp_extensions.nvim'

  " FZF
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/fzf.vim'
  Plug 'airblade/vim-rooter'

  " Ale
  " Plug 'w0rp/ale'

  " Surround
  Plug 'tpope/vim-surround'

  " Editor Config
  Plug 'editorconfig/editorconfig-vim'

  " Comment code
  Plug 'tpope/vim-commentary'

  " File utilities
  Plug 'tpope/vim-eunuch'

  "" Ranger
  " "Plug 'kevinhwang91/rnvimr'
  " Colorizer
  Plug 'norcalli/nvim-colorizer.lua'
  " Rainbow
  Plug 'junegunn/rainbow_parentheses.vim'
  " Formatting
  Plug 'sbdchd/neoformat'
  " Scratchpad
  Plug 'metakirby5/codi.vim'

  " Git
  Plug 'mhinz/vim-signify'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-rhubarb'
  Plug 'junegunn/gv.vim'

  " Start page
  Plug 'mhinz/vim-startify'

  " Quick scope
  Plug 'unblevable/quick-scope'

  " Which key
  Plug 'liuchengxu/vim-which-key'

  " Zen mode
  Plug 'junegunn/goyo.vim'
  Plug 'junegunn/limelight.vim'

  " Display indent
  Plug 'Yggdroot/indentLine'
call plug#end()

