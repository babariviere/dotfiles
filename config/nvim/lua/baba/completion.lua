vim.api.nvim_command [[autocmd BufEnter * lua require'completion'.on_attach{
  enable_auto_hover = 1,
  enable_auto_signature = 1
}]]

vim.api.nvim_set_keymap("i", "<c-space>", "completion#trigger_completion()",
  {noremap = true, silent = true, expr = true})

-- TODO(babariviere): only allow path in string or comments
vim.g.completion_chain_complete_list = {
  {complete_items = {"lsp", "snippet"}},
  {complete_items = {"path"}},
  {mode = "<c-p>"},
  {mode = "<c-n>"}
}

vim.g.completion_customize_lsp_label = {
  Function = " [function]",
  Method = " [method]",
  Reference = " [refrence]",
  Enum = " [enum]",
  Field = "ﰠ [field]",
  Keyword = " [key]",
  Variable = " [variable]",
  Folder = " [folder]",
  Snippet = " [snippet]",
  Operator = " [operator]",
  Module = " [module]",
  Text = "ﮜ[text]",
  Class = " [class]",
  Interface = " [interface]"
}

