local completion = require('completion')
local diagnostic = require('diagnostic')
local lsp_status = require('lsp-status')

local on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  completion.on_attach({
    enable_auto_hover = 1,
    enable_auto_signature = 1
  })
  diagnostic.on_attach()
  lsp_status.on_attach(client, bufnr)

  local opts = {noremap = true, silent = true}
  vim.api.nvim_buf_set_keymap(0, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', '<c-s>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'gTD', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'ga', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  -- vim.api.nvim_buf_set_keymap(0, 'n', '<leader>e', '<cmd>lua vim.lsp.util.show_line_diagnostics()<CR>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', ']e', '<cmd>NextDiagnosticCycle<cr>', opts)
  vim.api.nvim_buf_set_keymap(0, 'n', '[e', '<cmd>PrevDiagnosticCycle<cr>', opts)

  vim.api.nvim_command('inoremap <silent><expr> <c-space> completion#trigger_completion()')

  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_command('augroup lsp_aucmds')
    vim.api.nvim_command('au CursorHold <buffer> lua vim.lsp.buf.document_highlight()')
    vim.api.nvim_command('au CursorMoved <buffer> lua vim.lsp.buf.clear_references()')
    vim.api.nvim_command('augroup END')
  end

  vim.api.nvim_command("au CursorHold <buffer> lua vim.lsp.util.show_line_diagnostics()")
end

lsp_status.register_progress()
lsp_status.config({
  status_symbol = 'lsp',
  indicator_errors = 'e',
  indicator_warnings = 'w',
  indicator_info = 'i',
  indicator_hint = 'h',
  indicator_ok = 'ok',
  spinner_frames = { '⣾', '⣽', '⣻', '⢿', '⡿', '⣟', '⣯', '⣷' },
})

local servers = {
  elixirls = {},
  gopls = {},
  rust_analyzer = {},
  sumneko_lua = {
    settings = {
      Lua = {
        diagnostics = {
            globals = { 'vim' }
        },
        workflows = {
          library = {
            ['/usr/local/opt/neovim/share/nvim/runtime'] = true
          }
        }
      }
    }
  },
  vimls = {}
}

local lsp = require 'nvim_lsp'

for name, config in pairs(servers) do
  config.on_attach = on_attach
  config.capabilities = vim.tbl_extend('keep', config.capabilities or {}, lsp_status.capabilities)
  if lsp_status.extensions[name] then
    config.callbacks = lsp_status.extensions[name].setup()
  end
  lsp[name].setup(config)
end

vim.o.completeopt = 'menuone,noinsert,noselect'

vim.g.diagnostic_enable_virtual_text = 1
vim.g.diagnostic_trimmed_virtual_text = '40'
vim.g.diagnostic_insert_delay = 1
vim.g.diagnostic_virtual_text_prefix = '» '
vim.g.space_before_virtual_text = 5
vim.g.completion_chain_complete_list = {
  {complete_items = {'lsp', 'snippet'}},
}
