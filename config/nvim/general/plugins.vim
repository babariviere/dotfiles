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
  Plug 'dracula/vim', { 'as': 'dracula' }

  " Status bar
  " Plug 'itchyny/lightline.vim'
  " Plug 'mengelbrecht/lightline-bufferline'
  " Plug 'vim-airline/vim-airline'
  " Plug 'vim-airline/vim-airline-themes'
  Plug 'glepnir/galaxyline.nvim'
  Plug 'akinsho/nvim-bufferline.lua'
  Plug 'kyazdani42/nvim-web-devicons'

  " Languages
  " Plug 'sheerun/vim-polyglot'
  Plug 'LnL7/vim-nix'
  Plug 'habamax/vim-asciidoctor'
  Plug 'elixir-editors/vim-elixir'
  Plug 'jparise/vim-graphql'

  Plug 'nvim-treesitter/nvim-treesitter'

  " Auto Pairs
  Plug 'jiangmiao/auto-pairs'

  " LSP nvim
  Plug 'neovim/nvim-lspconfig'
  Plug 'nvim-lua/completion-nvim'
  Plug 'nvim-lua/lsp-status.nvim'
  Plug 'tjdevries/lsp_extensions.nvim'
  Plug 'RishabhRD/popfix'
  Plug 'RishabhRD/nvim-lsputils'

  " Lua nvim
  Plug 'nvim-lua/popup.nvim'
  Plug 'nvim-lua/plenary.nvim'
  Plug 'nvim-lua/telescope.nvim'

  " Flow
  Plug 'babariviere/flow.nvim'

  " Snippets
  Plug 'SirVer/ultisnips'
  Plug 'honza/vim-snippets'

  " Project root
  Plug 'airblade/vim-rooter'

  " Surround
  Plug 'blackcauldron7/surround.nvim'

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
  " Plug 'sbdchd/neoformat'
  Plug 'mhartington/formatter.nvim'

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

  " Language tool
  Plug 'rhysd/vim-grammarous'

  " Direnv
  Plug 'direnv/direnv.vim'

  " Neuron
  Plug 'junegunn/fzf'
  Plug 'junegunn/fzf.vim'
  Plug 'fiatjaf/neuron.vim'

  " Easy align
  Plug 'junegunn/vim-easy-align'
call plug#end()

