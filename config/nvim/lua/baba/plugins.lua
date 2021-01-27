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

    -- Themes
    use "ntk148v/vim-horizon"
    use "Th3Whit3Wolf/one-nvim"
    use "ayu-theme/ayu-vim"
    use "drewtempelmeyer/palenight.vim"
    use {"dracula/vim", as = "dracula"}
    use "NLKNguyen/papercolor-theme"
    use "sainnhe/edge"

    -- Status bar
    -- pkg {'itchyny/lightline.vim', as = 'lightline'}
    -- use 'mengelbrecht/lightline-bufferline'
    -- use 'vim-airline/vim-airline'
    -- use 'vim-airline/vim-airline-themes'
    use {"glepnir/galaxyline.nvim", config = [[require'baba.statusline']]}
    -- pkg {"akinsho/nvim-bufferline.lua", as = "bufferline"}
    use {"romgrk/barbar.nvim", as = "barbar"}
    use "kyazdani42/nvim-web-devicons"

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

    -- Auto Pairs
    -- pkg {'jiangmiao/auto-pairs', as = 'autopairs'}
    pkg {"cohama/lexima.vim", as = "lexima"}

    -- LSP nvim
    use {"neovim/nvim-lspconfig", config = [[require'baba.lsp']]}
    use {"nvim-lua/completion-nvim", config = [[require'baba.completion']]}
    use "nvim-lua/lsp-status.nvim"
    use "tjdevries/lsp_extensions.nvim"
    use "RishabhRD/popfix"
    use {"RishabhRD/nvim-lsputils", requires = {{"RishabhRD/popfix", opt = true}}}
    use {"glepnir/lspsaga.nvim", requires = {"neovim/nvim-lspconfig"}}

    -- DAP
    use {{"mfussenegger/nvim-dap", config = [[require'baba.dap']]}, "theHamsta/nvim-dap-virtual-text"}

    -- Lua nvim
    use "nvim-lua/plenary.nvim"
    use {"nvim-lua/popup.nvim", requires = {{"nvim-lua/plenary.nvim", opt = true}}}
    pkg {
      "nvim-telescope/telescope.nvim",
      as = "telescope",
      requires = {
        {"nvim-lua/popup.nvim", opt = true},
        {"nvim-lua/plenary.nvim", opt = true},

        -- extensions
        {"nvim-telescope/telescope-github.nvim"}
      }
    }
    -- use 'rafcamlet/nvim-luapad'

    -- Flow
    pkg {"babariviere/flow.nvim", as = "flow", requires = {"nvim-telescope/telescope.nvim", opt = true}}

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
    -- Colorizer
    use {"norcalli/nvim-colorizer.lua", config = [[require 'baba.colorizer']]}
    -- Rainbow
    use "junegunn/rainbow_parentheses.vim"
    -- Formatting
    -- use 'sbdchd/neoformat'
    use {"mhartington/formatter.nvim", config = [[require'baba.formatter']]}

    -- Scratchpad
    use "metakirby5/codi.vim"

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
      },
      {"f-person/git-blame.nvim", config = [[require'baba.git']]}
    }
    use {
      "pwntester/octo.nvim",
      requires = {"nvim-lua/popup.nvim", "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim"}
    }

    -- Start page
    use "mhinz/vim-startify"

    -- Quick scope
    pkg {"unblevable/quick-scope", as = "quickscope"}

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
        local ok, float_win = pcall(function()
          return require("plenary.window.float").percentage_range_window(0.8, 0.8)
        end)

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
