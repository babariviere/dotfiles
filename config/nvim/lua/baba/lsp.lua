local statusline = require("baba.statusline")
local lsp_status = require("lsp-status")

local on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  lsp_status.on_attach(client, bufnr)
  statusline.on_attach(client, bufnr)

  local opts = {noremap = true, silent = true}
  vim.api.nvim_buf_set_keymap(0, "n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "<c-s>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "gTD", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "ga", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "ge", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "gE", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "]e", "<cmd>lua vim.lsp.diagnostic.goto_next()<cr>", opts)
  vim.api.nvim_buf_set_keymap(0, "n", "[e", "<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>", opts)

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

lsp_status.register_progress()

local sumneko_root = os.getenv("HOME") .. "/src/github.com/sumneko/lua-language-server"
local sumneko_bin = sumneko_root .. "/bin/macOS/lua-language-server"

local lsp = require "lspconfig"

local servers = {
  diagnosticls = {
    filetypes = {"asciidoc", "markdown", "gitcommit", "sh"},
    init_options = {
      linters = {
        shellcheck = {
          command = "shellcheck",
          debounce = 100,
          args = {"--format=gcc", "-"},
          offsetLine = 0,
          offsetColumn = 0,
          sourceName = "shellcheck",
          formatLines = 1,
          formatPattern = {
            "^[^:]+:(\\d+):(\\d+):\\s+([^:]+):\\s+(.*)$",
            {line = 1, column = 2, message = 4, security = 3}
          },
          securities = {error = "error", warning = "warning", note = "info"}
        },
        languagetool = {
          command = "languagetool",
          debounce = 200,
          args = {"-d", "EN_QUOTES", "-m", "fr-FR", "-l", "en-US", "-"},
          offsetLine = 0,
          offsetColumn = 0,
          sourceName = "languagetool",
          formatLines = 2,
          formatPattern = {
            "^\\d+?\\.\\)\\s+Line\\s+(\\d+),\\s+column\\s+(\\d+),\\s+([^\\n]+)\nMessage:\\s+(.*)$",
            {line = 1, column = 2, message = {4, 3}}
          }
        }
      },
      filetypes = {sh = "shellcheck", asciidoc = "languagetool", markdown = "languagetool", gitcommit = "languagetool"},
      -- Use format.nvim instead
      formatters = {},
      formatFiletypes = {}
    }
  },

  elixirls = {cmd = {os.getenv("HOME") .. "/src/github.com/elixir-lsp/elixir-ls/release/language_server.sh"}},

  gopls = {},

  pyls_ms = {
    cmd = {"dotnet", "exec", os.getenv("HOME") .. "/.cache/nvim/lspconfig/pyls_ms/Microsoft.Python.LanguageServer.dll"},
    init_options = {interpreter = {properties = {InterpreterPath = "/usr/local/bin/python3", Version = "3.9"}}}
  },

  rust_analyzer = {},

  scry = {cmd = {os.getenv("HOME") .. "/src/github.com/crystal-lang-tools/scry/bin/scry"}},

  sumneko_lua = {cmd = {sumneko_bin, "-E", sumneko_root .. "/main.lua"}},

  tsserver = {
    -- default_config = {
    --   bin_dir = '/usr/local/bin',
    cmd = {"typescript-language-server", "--stdio"}
    -- }
  }
}

local function find_lsp_ancestor(startpath)
  return lsp.util.search_ancestors(startpath, function(path)
    if lsp.util.path.is_file(lsp.util.path.join(path, ".lsp.lua")) then
      return path
    end
  end)
end

for name, config in pairs(servers) do
  config.on_attach = on_attach
  config.on_new_config = on_new_config
  config.capabilities = vim.tbl_extend("keep", config.capabilities or {}, lsp_status.capabilities)
  local default_config = lsp[name].document_config.default_config
  config.root_dir = function(fname)
    return find_lsp_ancestor(fname) or default_config.root_dir(fname)
  end
  lsp[name].setup(config)
end

vim.o.completeopt = "menuone,noinsert,noselect"

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  -- Enable underline, use default values
  underline = true,
  -- Enable virtual text
  virtual_text = {prefix = "»"},
  signs = true,
  -- Disable a feature
  update_in_insert = false
})

vim.lsp.handlers["textDocument/codeAction"] = require"lsputil.codeAction".code_action_handler
vim.lsp.handlers["textDocument/references"] = require"lsputil.locations".references_handler
vim.lsp.handlers["textDocument/definition"] = require"lsputil.locations".definition_handler
vim.lsp.handlers["textDocument/declaration"] = require"lsputil.locations".declaration_handler
vim.lsp.handlers["textDocument/typeDefinition"] = require"lsputil.locations".typeDefinition_handler
vim.lsp.handlers["textDocument/implementation"] = require"lsputil.locations".implementation_handler
vim.lsp.handlers["textDocument/documentSymbol"] = require"lsputil.symbols".document_handler
vim.lsp.handlers["workspace/symbol"] = require"lsputil.symbols".workspace_handler

vim.g.lsp_utils_location_opts = {keymaps = {n = {["<C-j>"] = "j", ["<C-k>"] = "k"}}}
vim.g.lsp_utils_symbols_opts = {keymaps = {n = {["<C-j>"] = "j", ["<C-k>"] = "k"}}}
vim.g.lsp_utils_codeaction_opts = {keymaps = {n = {["<C-j>"] = "j", ["<C-k>"] = "k"}}}

vim.g.space_before_virtual_text = 5
