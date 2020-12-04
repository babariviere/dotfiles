local lsp_status = require('lsp-status')

local on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  lsp_status.on_attach(client, bufnr)

  local opts = {noremap = true, silent = true}
  vim.api.nvim_buf_set_keymap(0, 'n', 'gD',
                              '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'gd',
                              '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>',
                              opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'gi',
                              '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', '<c-s>',
                              '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'gTD',
                              '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', '<leader>rn',
                              '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'gr',
                              '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'ga',
                              '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'ge',
                              '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>',
                              opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'gE',
                              '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>',
                              opts)
  vim.api.nvim_buf_set_keymap(0, 'n', ']e',
                              '<cmd>lua vim.lsp.diagnostic.goto_next()<cr>',
                              opts)
  vim.api.nvim_buf_set_keymap(0, 'n', '[e',
                              '<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>',
                              opts)

  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_command('augroup lsp_aucmds')
    vim.api.nvim_command(
      'au CursorHold <buffer> lua vim.lsp.buf.document_highlight()')
    vim.api.nvim_command(
      'au CursorMoved <buffer> lua vim.lsp.buf.clear_references()')
    vim.api.nvim_command('augroup END')
  end

  vim.b.lsp_client_name = client.name

  -- vim.api.nvim_command("au CursorHold <buffer> lua vim.lsp.diagnostic.show_line_diagnostics()")
end

local on_new_config = function(config, root_dir)
  local lsp_config = root_dir .. '/.lsp.json'

  if not vim.fn.filereadable(lsp_config) then return end

  local out = vim.fn.readfile(lsp_config)
  local raw = vim.fn.json_decode(out)

  local decoder = {}
  decoder.decode = function(tbl)
    if type(tbl) ~= 'table' then return tbl end
    for k, v in pairs(tbl) do
      if type(k) ~= 'string' then goto continue end
      local i = k:find('%.')
      if i then
        local a = k:sub(0, i - 1)
        local b = k:sub(i + 1)
        tbl[k] = nil

        tbl[a] = tbl[a] or {}
        tbl[a][b] = v
        tbl[a] = decoder.decode(tbl[a])
      else
        tbl[k] = decoder.decode(v)
      end
      ::continue::
    end
    return tbl
  end

  local settings = {settings = decoder.decode(raw)}

  local new_config = vim.tbl_extend('force', config, settings)

  for k, v in pairs(new_config) do config[k] = v end
end

lsp_status.register_progress()
-- lsp_status.config({
--   status_symbol = 'lsp',
--   indicator_errors = 'e',
--   indicator_warnings = 'w',
--   indicator_info = 'i',
--   indicator_hint = 'h',
--   indicator_ok = 'ok',
--   spinner_frames = { '⣾', '⣽', '⣻', '⢿', '⡿', '⣟', '⣯', '⣷' },
-- })

local servers = {
  elixirls = {},
  gopls = {},
  pyls_ms = {},
  rust_analyzer = {},
  sumneko_lua = {
    settings = {
      Lua = {
        runtime = {
          -- Get the language server to recognize LuaJIT globals like `jit` and `bit`
          version = 'LuaJIT',
          -- Setup your lua path
          path = vim.split(package.path, ';')
        },
        diagnostics = {
          -- Get the language server to recognize the `vim` global
          globals = {'vim'}
        },
        workspace = {
          -- Make the server aware of Neovim runtime files
          library = {
            [vim.fn.expand('$VIMRUNTIME/lua')] = true,
            [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true
          }
        }
      }
    }
  }
}

local lsp = require 'lspconfig'

for name, config in pairs(servers) do
  config.on_attach = on_attach
  config.on_new_config = on_new_config
  config.capabilities = vim.tbl_extend('keep', config.capabilities or {},
                                       lsp_status.capabilities)
  if lsp_status.extensions[name] then
    config.callbacks = lsp_status.extensions[name].setup()
  end
  lsp[name].setup(config)
end

vim.o.completeopt = 'menuone,noinsert,noselect'

vim.lsp.handlers['textDocument/publishDiagnostics'] =
  vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
    -- Enable underline, use default values
    underline = true,
    -- Enable virtual text
    virtual_text = {prefix = '»'},
    signs = true,
    -- Disable a feature
    update_in_insert = false
  })

vim.lsp.handlers['textDocument/codeAction'] =
  require'lsputil.codeAction'.code_action_handler
vim.lsp.handlers['textDocument/references'] =
  require'lsputil.locations'.references_handler
vim.lsp.handlers['textDocument/definition'] =
  require'lsputil.locations'.definition_handler
vim.lsp.handlers['textDocument/declaration'] =
  require'lsputil.locations'.declaration_handler
vim.lsp.handlers['textDocument/typeDefinition'] =
  require'lsputil.locations'.typeDefinition_handler
vim.lsp.handlers['textDocument/implementation'] =
  require'lsputil.locations'.implementation_handler
vim.lsp.handlers['textDocument/documentSymbol'] =
  require'lsputil.symbols'.document_handler
vim.lsp.handlers['workspace/symbol'] =
  require'lsputil.symbols'.workspace_handler

vim.g.lsp_utils_location_opts = {
  keymaps = {n = {['<C-j>'] = 'j', ['<C-k>'] = 'k'}}
}
vim.g.lsp_utils_symbols_opts = {
  keymaps = {n = {['<C-j>'] = 'j', ['<C-k>'] = 'k'}}
}
vim.g.lsp_utils_codeaction_opts = {
  keymaps = {n = {['<C-j>'] = 'j', ['<C-k>'] = 'k'}}
}

vim.g.space_before_virtual_text = 5
