local statusline = require "baba.statusline"
local lsp_status = require "lsp-status"

local on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  lsp_status.on_attach(client, bufnr)
  statusline.on_attach(client, bufnr)

  local opts = {noremap = true, silent = true}
  vim.api.nvim_buf_set_keymap(0, "n", "gnD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "gnd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "K", "<cmd>lua require('lspsaga.hover').render_hover_doc()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "<c-s>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "gTD", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "gR", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "ga", "<cmd>lua require('lspsaga.codeaction').code_action()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "ge", "<cmd>lua require'lspsaga.diagnostic'.show_line_diagnostics()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "gE", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "]e", "<cmd>lua vim.lsp.diagnostic.goto_next()<cr>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "[e", "<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "<leader>ca", "<cmd>lua require('lspsaga.codeaction').code_action()<cr>", opts)

  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_command("augroup lsp_aucmds")
    vim.api.nvim_command("au CursorHold <buffer> lua vim.lsp.buf.document_highlight()")
    vim.api.nvim_command("au CursorMoved <buffer> lua vim.lsp.buf.clear_references()")
    vim.api.nvim_command("augroup END")
  end

  vim.b.lsp_client_name = client.name

  -- vim.api.nvim_command("au CursorHold <buffer> lua vim.lsp.diagnostic.show_line_diagnostics()")
end

local on_new_config = function(config, root_dir)
  local lsp_config = root_dir .. "/.lsp.lua"

  if not vim.fn.filereadable(lsp_config) then
    return
  end

  local settings = {settings = loadfile(lsp_config)()}

  local new_config = vim.tbl_extend("force", config, settings)

  for k, v in pairs(new_config) do
    config[k] = v
  end
end

-- Setup autocmd for lsp.lua files
vim.api.nvim_command [[ autocmd BufNewFile .lsp.lua 0r ~/.config/nvim/templates/lsp.lua ]]

lsp_status.register_progress()

local sumneko_root = os.getenv("HOME") .. "/src/github.com/sumneko/lua-language-server"
local sumneko_bin = sumneko_root .. "/bin/macOS/lua-language-server"

local lsp = require "lspconfig"

local servers = {
  clojure_lsp = {},
  -- diagnosticls = {
  --   filetypes = {"asciidoc", "markdown", "gitcommit", "sh"},
  --   init_options = {
  --     linters = {
  --       shellcheck = {
  --         command = "shellcheck",
  --         debounce = 100,
  --         args = {"--format=gcc", "-"},
  --         offsetLine = 0,
  --         offsetColumn = 0,
  --         sourceName = "shellcheck",
  --         formatLines = 1,
  --         formatPattern = {
  --           "^[^:]+:(\\d+):(\\d+):\\s+([^:]+):\\s+(.*)$",
  --           {line = 1, column = 2, message = 4, security = 3}
  --         },
  --         securities = {error = "error", warning = "warning", note = "info"}
  --       },
  --       languagetool = {
  --         command = "languagetool",
  --         debounce = 200,
  --         args = {"-d", "EN_QUOTES", "-m", "fr", "-l", "en-US", "-"},
  --         offsetLine = 0,
  --         offsetColumn = 0,
  --         sourceName = "languagetool",
  --         formatLines = 2,
  --         formatPattern = {
  --           "^\\d+?\\.\\)\\s+Line\\s+(\\d+),\\s+column\\s+(\\d+),\\s+([^\\n]+)\nMessage:\\s+(.*)$",
  --           {line = 1, column = 2, message = {4, 3}}
  --         }
  --       }
  --     },
  --     filetypes = {
  --       sh = "shellcheck",
  --       asciidoc = "languagetool",
  --       markdown = "languagetool",
  --       gitcommit = "languagetool"
  --     },
  --     -- Use format.nvim instead
  --     formatters = {},
  --     formatFiletypes = {}
  --   }
  -- },
  elixirls = {cmd = {os.getenv("HOME") .. "/src/github.com/elixir-lsp/elixir-ls/release/language_server.sh"}},
  gopls = {},
  pyls_ms = {
    cmd = {"dotnet", "exec", os.getenv("HOME") .. "/.cache/nvim/lspconfig/pyls_ms/Microsoft.Python.LanguageServer.dll"},
    init_options = {interpreter = {properties = {InterpreterPath = "/usr/local/bin/python3", Version = "3.9"}}}
  },
  rust_analyzer = {},
  scry = {cmd = {os.getenv("HOME") .. "/src/github.com/crystal-lang-tools/scry/bin/scry"}},
  sumneko_lua = {
    cmd = {sumneko_bin, "-E", sumneko_root .. "/main.lua"},
    filetypes = {"lua"},
    settings = {
      completion = {
        keywordSnippet = "Disable"
      },
      workspace = {
        maxPreload = 1000,
        preloadFileSize = 1000
      }
    }
  },
  tsserver = {
    -- default_config = {
    --   bin_dir = '/usr/local/bin',
    cmd = {"typescript-language-server", "--stdio"}
    -- }
  }
}

-- Patch upstream iterate_parents.
lsp.util.path.iterate_parents = function(path)
  local abs_path = vim.loop.fs_realpath(path)
  if not abs_path then
    local parent = lsp.util.path.dirname(path)
    path = vim.loop.fs_realpath(parent)
  else
    path = abs_path
  end
  local function it(_, v)
    if not v then
      return
    end
    if v == "/" then
      return
    end
    return lsp.util.path.dirname(v), path
  end
  return it, path, path
end

local function find_lsp_ancestor(startpath)
  return lsp.util.search_ancestors(
    startpath,
    function(path)
      if lsp.util.path.is_file(lsp.util.path.join(path, ".lsp.lua")) then
        return path
      end
    end
  )
end

for name, config in pairs(servers) do
  config.on_attach = on_attach
  config.on_new_config = on_new_config
  config.capabilities = vim.tbl_extend("keep", config.capabilities or {}, lsp_status.capabilities)
  local default_config = lsp[name].document_config.default_config
  config.root_dir = function(fname)
    return find_lsp_ancestor(fname) or default_config.root_dir(fname)
  end

  -- Use new incremental sync by default: https://github.com/neovim/neovim/pull/14079
  if not config.flags then
    config.flags = {}
  end
  config.flags.allow_incremental_sync = true

  lsp[name].setup(config)
end

vim.o.completeopt = "menuone,noinsert,noselect"

vim.lsp.handlers["textDocument/publishDiagnostics"] =
  vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics,
  {
    -- Enable underline, use default values
    underline = true,
    -- Enable virtual text
    virtual_text = {prefix = "»"},
    signs = true,
    -- Disable a feature
    update_in_insert = false
  }
)

vim.g.space_before_virtual_text = 5

-- LSP Saga
local saga = require "lspsaga"
saga.init_lsp_saga {
  code_action_prompt = {
    enable = true,
    sign = true,
    sign_priority = 20,
    virtual_text = false
  }
}
