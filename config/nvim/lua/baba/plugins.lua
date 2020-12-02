-- Only required if you have packer in your `opt` pack
local packer_exists = pcall(vim.cmd, [[packadd packer.nvim]])

if not packer_exists then
  -- TODO: Maybe handle windows better?
  if vim.fn.input("Download Packer? (y for yes) ") ~= "y" then
    return
  end

  local directory = string.format(
    '%s/site/pack/packer/opt/',
    vim.fn.stdpath('data')
  )

  vim.fn.mkdir(directory, 'p')

  local out = vim.fn.system(string.format(
      'git clone %s %s',
      'https://github.com/wbthomason/packer.nvim',
      directory .. '/packer.nvim'
    ))

  print(out)
  print("Downloading packer.nvim...")

  return
end

return require('packer').startup {
    function(use)
      -- Packer can manage itself as an optional plugin
      use {'wbthomason/packer.nvim', opt = true}

      -- Themes
      use 'ntk148v/vim-horizon'
      use 'rakr/vim-one'
      use 'ayu-theme/ayu-vim'
      use 'drewtempelmeyer/palenight.vim'
      use {'dracula/vim', as = 'dracula'}

      -- Status bar
      -- use 'itchyny/lightline.vim'
      -- use 'mengelbrecht/lightline-bufferline'
      -- use 'vim-airline/vim-airline'
      -- use 'vim-airline/vim-airline-themes'
      use 'glepnir/galaxyline.nvim'
      use 'akinsho/nvim-bufferline.lua'
      use 'kyazdani42/nvim-web-devicons'

      -- Languages
      -- use 'sheerun/vim-polyglot'
      use 'LnL7/vim-nix'
      use 'habamax/vim-asciidoctor'
      use 'elixir-editors/vim-elixir'
      use 'jparise/vim-graphql'
      use 'ARM9/arm-syntax-vim'
      use 'mustache/vim-mustache-handlebars'

      use 'nvim-treesitter/nvim-treesitter'

      -- Auto Pairs
      use 'jiangmiao/auto-pairs'

      -- LSP nvim
      use 'neovim/nvim-lspconfig'
      use 'nvim-lua/completion-nvim'
      use 'nvim-lua/lsp-status.nvim'
      use 'tjdevries/lsp_extensions.nvim'
      use 'RishabhRD/popfix'
      use {'RishabhRD/nvim-lsputils', requires = {{'RishabhRD/popfix', opt = true}}}

      -- Lua nvim
      use 'nvim-lua/plenary.nvim'
      use {'nvim-lua/popup.nvim', requires = {{'nvim-lua/plenary.nvim', opt = true}}}
      use {
          'nvim-telescope/telescope.nvim',
          requires = {
            {'nvim-lua/popup.nvim', opt = true},
            {'nvim-lua/plenary.nvim', opt = true}
          }
        }
      -- use 'rafcamlet/nvim-luapad'

      -- Flow
      use 'babariviere/flow.nvim'

      -- Snippets
      -- use 'SirVer/ultisnips'
      -- use 'honza/vim-snippets'
      use 'norcalli/snippets.nvim'

      -- Project root
      use 'airblade/vim-rooter'

      -- Surround
      use 'blackcauldron7/surround.nvim'

      -- Editor Config
      use 'editorconfig/editorconfig-vim'

      -- Comment code
      use 'tpope/vim-commentary'

      -- File utilities
      use 'tpope/vim-eunuch'

      ---- Ranger
      -- --use 'kevinhwang91/rnvimr'
      -- Colorizer
      use 'norcalli/nvim-colorizer.lua'
      -- Rainbow
      use 'junegunn/rainbow_parentheses.vim'
      -- Formatting
      -- use 'sbdchd/neoformat'
      use 'mhartington/formatter.nvim'

      -- Scratchpad
      use 'metakirby5/codi.vim'

      -- Git
      use 'mhinz/vim-signify'
      use 'tpope/vim-fugitive'
      use 'tpope/vim-rhubarb'
      use 'junegunn/gv.vim'

      -- Start page
      use 'mhinz/vim-startify'

      -- Quick scope
      use 'unblevable/quick-scope'

      -- Which key
      use 'liuchengxu/vim-which-key'

      -- Zen mode
      use 'junegunn/goyo.vim'
      use 'junegunn/limelight.vim'

      -- Language tool
      use 'rhysd/vim-grammarous'

      -- Direnv
      use 'direnv/direnv.vim'

      -- Neuron
      use 'junegunn/fzf'
      use 'junegunn/fzf.vim'
      use 'fiatjaf/neuron.vim'

      -- Easy align
      use 'junegunn/vim-easy-align'
    end,
    config = {
      display = {
        open_fn = function(name)
          -- Can only use plenary when we have our plugins.
          -- We can only get plenary when we don't have our plugins ;)
          local ok, float_win = pcall(function()
              return require('plenary.window.float').percentage_range_window(0.8, 0.8)
            end)

          if not ok then
            vim.cmd [[65vnew  [packer] ]]

            return vim.api.nvim_get_current_win(), vim.api.nvim_get_current_buf()
          end

          local bufnr = float_win.bufnr
          local win = float_win.win_id

          vim.api.nvim_buf_set_name(bufnr, name)
          vim.api.nvim_win_set_option(win, 'winblend', 10)

          return win, bufnr
        end
      },
    }
  }
