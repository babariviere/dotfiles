vim.o.completeopt = "menu,menuone,noselect"

require "compe".setup {
  enabled = true,
  autocomplete = true,
  debug = false,
  min_length = 1,
  preselect = "enable",
  throttle_time = 80,
  source_timeout = 200,
  incomplete_delay = 400,
  max_abbr_width = 100,
  max_kind_width = 100,
  max_menu_width = 100,
  documentation = true,
  source = {
    path = true,
    buffer = true,
    calc = true,
    conjure = true,
    vsnip = false,
    nvim_lsp = true,
    nvim_lua = true,
    spell = true,
    tags = true,
    snippets_nvim = true,
    treesitter = true
  }
}

vim.g.lexima_no_default_rules = true
vim.fn["lexima#set_default_rules"]()
local opts = {noremap = true, silent = true, expr = true}
vim.api.nvim_set_keymap("i", "<C-Space>", "compe#complete()", opts)
vim.api.nvim_set_keymap("i", "<CR>", 'compe#confirm(lexima#expand("<LT>CR>", "i"))', opts)
vim.api.nvim_set_keymap("i", "<C-e>", "compe#close()", opts)
vim.api.nvim_set_keymap("i", "<C-f>", 'compe#scroll({ "delta": +4 })', opts)
vim.api.nvim_set_keymap("i", "<C-d>", 'compe#scroll({ "delta": -4 })', opts)
