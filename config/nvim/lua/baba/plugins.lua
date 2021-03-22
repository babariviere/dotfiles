-- Only required if you have packer in your `opt` pack
local packer_exists = pcall(vim.cmd, [[packadd packer.nvim]])

if not packer_exists then
  local install_path = os.getenv("HOME") .. "/.local/share/nvim/site/pack/packer/opt/packer.nvim"
  vim.cmd("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
  vim.cmd "packadd packer.nvim"
end

vim.cmd([[autocmd BufWritePost **/nvim/lua/baba/plugins.lua PackerCompile]])

return require("packer").startup {
  function(use)
    local pkg = function(package)
      local build_config = function(name)
        local _, _, pname = name:match("(.-)([^\\/]-%.?([^%.\\/]*))$")
        pname = pname:gsub("-", "_")
        pname = "baba.plugins." .. pname
        return [[require']] .. pname .. [[']]
      end

      if type(package) == "table" then
        local pname = package[1]
        package.config = build_config(package.as or pname)
        return use(package)
      else
        return use {package, config = build_config(package)}
      end
    end

    -- Packer can manage itself as an optional plugin
    use {"wbthomason/packer.nvim", opt = true}

    -- Fennel/Conjure/Lisp...
    -- use "guns/vim-sexp"
    -- use "tpope/vim-sexp-mappings-for-regular-people"
    -- pkg {"liquidz/vim-iced", as = "iced"}
    pkg "Olical/conjure"
    use {"Olical/aniseed", requires = {{"bakpakin/fennel.vim"}}}
    use {"Olical/nvim-local-fennel", requires = {{"bakpakin/fennel.vim"}}}
    -- use "bhurlow/vim-parinfer"
    use {"eraserhd/parinfer-rust", run = "cargo build --release"}

    -- Themes
    -- use "ntk148v/vim-horizon"
    use "Th3Whit3Wolf/one-nvim"
    use "ayu-theme/ayu-vim"
    use "drewtempelmeyer/palenight.vim"
    use {"dracula/vim", as = "dracula"}
    use "NLKNguyen/papercolor-theme"
    use "sainnhe/edge"

    use "rktjmp/lush.nvim"

    -- Status bar
    -- pkg {'itchyny/lightline.vim', as = 'lightline'}
    -- use 'mengelbrecht/lightline-bufferline'
    -- use 'vim-airline/vim-airline'
    -- use 'vim-airline/vim-airline-themes'
    use {"glepnir/galaxyline.nvim", config = [[require'baba.statusline']]}
    -- pkg {"akinsho/nvim-bufferline.lua", as = "bufferline"}
    pkg {"romgrk/barbar.nvim", as = "barbar"}
    -- use "kyazdani42/nvim-web-devicons"
    use {
      "yamatsum/nvim-web-nonicons",
      requires = {"kyazdani42/nvim-web-devicons"}
    }

    -- Languages
    -- use 'sheerun/vim-polyglot'
    use "LnL7/vim-nix"
    pkg {"habamax/vim-asciidoctor", as = "asciidoctor"}
    use "elixir-editors/vim-elixir"
    use "jparise/vim-graphql"
    use "ARM9/arm-syntax-vim"
    use "mustache/vim-mustache-handlebars"
    use "vim-crystal/vim-crystal"
    use "earthly/earthly.vim"

    use {
      "nvim-treesitter/nvim-treesitter",
      run = function()
        vim.api.nvim_command [[TSUpdate]]
      end,
      config = [[require'baba.treesitter']]
    }
    use {"nvim-treesitter/playground"}
    use {"nvim-treesitter/nvim-treesitter-textobjects"}
    use {"nvim-treesitter/nvim-treesitter-refactor"}
    use {"nvim-treesitter/nvim-tree-docs", requires = {{"Olical/aniseed"}}}
    use {"p00f/nvim-ts-rainbow"}

    -- Auto Pairs
    -- pkg {'jiangmiao/auto-pairs', as = 'autopairs'}
    pkg {"cohama/lexima.vim", as = "lexima"}

    -- LSP nvim
    use {"neovim/nvim-lspconfig", config = [[require'baba.lsp']]}
    use {"nvim-lua/completion-nvim", config = [[require'baba.completion']]}
    -- pkg "hrsh7th/nvim-compe"
    use "nvim-lua/lsp-status.nvim"
    use "tjdevries/lsp_extensions.nvim"
    use {"glepnir/lspsaga.nvim", requires = {"neovim/nvim-lspconfig"}}
    -- pkg {"kosayoda/nvim-lightbulb", as = "lightbulb"}

    -- DAP
    use {{"mfussenegger/nvim-dap", config = [[require'baba.dap']]}, "theHamsta/nvim-dap-virtual-text"}

    -- Lua nvim
    use "nvim-lua/plenary.nvim"
    use {"nvim-lua/popup.nvim", requires = {{"nvim-lua/plenary.nvim", opt = true}}}
    use {
      "nvim-telescope/telescope.nvim",
      config = [[require'baba.plugins.telescope']],
      requires = {
        {"nvim-lua/popup.nvim", opt = true},
        {"nvim-lua/plenary.nvim", opt = true},
        -- extensions
        {"nvim-telescope/telescope-github.nvim"}
      }
    }
    -- use 'rafcamlet/nvim-luapad'

    -- Flow
    pkg {"babariviere/flow.nvim", as = "flow"}

    -- Snippets
    -- use 'SirVer/ultisnips'
    -- use 'honza/vim-snippets'
    use {"norcalli/snippets.nvim", config = [[require'baba.snippets']]}

    -- Project root
    pkg {"airblade/vim-rooter", as = "rooter"}

    -- Surround
    pkg {"blackcauldron7/surround.nvim", as = "surround"}

    -- Editor Config
    use "editorconfig/editorconfig-vim"

    -- Comment code
    use "tpope/vim-commentary"

    -- File utilities
    use "tpope/vim-eunuch"

    ---- Ranger
    -- --use 'kevinhwang91/rnvimr'
    --- Colorizer
    -- use {"norcalli/nvim-colorizer.lua", config = [[require 'baba.colorizer']]}
    pkg {"RRethy/vim-hexokinase", as = "hexokinase", run = "make hexokinase"}

    -- Rainbow
    -- use "junegunn/rainbow_parentheses.vim"

    -- Formatting
    -- use 'sbdchd/neoformat'
    use {"mhartington/formatter.nvim", config = [[require'baba.formatter']]}

    -- Scratchpad
    -- use "metakirby5/codi.vim"

    -- Git
    use {
      "tpope/vim-fugitive",
      "tpope/vim-rhubarb",
      "junegunn/gv.vim",
      -- {'mhinz/vim-signify', config = [[require'baba.git']]},
      {
        "lewis6991/gitsigns.nvim",
        config = function()
        end,
        requires = {"nvim-lua/plenary.nvim"}
      }
      -- {"f-person/git-blame.nvim", config = [[require'baba.git']]}
    }
    pkg {
      "pwntester/octo.nvim",
      as = "octo",
      requires = {"nvim-lua/popup.nvim", "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim"}
    }
    -- use "TimUntersberger/neogit"

    -- Start page
    use "mhinz/vim-startify"

    -- Quick scope
    pkg {"unblevable/quick-scope", as = "quickscope"}

    -- Quick fix
    use "kevinhwang91/nvim-bqf"

    -- Which key
    use "liuchengxu/vim-which-key"

    -- Zen mode
    pkg {"junegunn/goyo.vim", as = "goyo"}
    use "junegunn/limelight.vim"

    -- Language tool
    -- use 'rhysd/vim-grammarous'

    -- Direnv
    use "direnv/direnv.vim"

    -- Neuron
    use "junegunn/fzf"
    use "junegunn/fzf.vim"
    use "fiatjaf/neuron.vim"

    -- Easy align
    pkg {"junegunn/vim-easy-align", as = "easyalign"}
  end,
  config = {
    display = {
      open_fn = function(name)
        -- Can only use plenary when we have our plugins.
        -- We can only get plenary when we don't have our plugins ;)
        local ok, float_win =
          pcall(
          function()
            return require("plenary.window.float").percentage_range_window(0.8, 0.8)
          end
        )

        if not ok then
          vim.cmd [[65vnew  [packer] ]]

          return true, vim.api.nvim_get_current_win(), vim.api.nvim_get_current_buf()
        end

        local bufnr = float_win.bufnr
        local win = float_win.win_id

        vim.api.nvim_buf_set_name(bufnr, name)
        vim.api.nvim_win_set_option(win, "winblend", 10)

        return true, win, bufnr
      end
    }
  }
}
