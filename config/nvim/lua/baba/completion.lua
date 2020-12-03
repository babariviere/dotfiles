vim.api.nvim_command [[autocmd BufEnter * lua require'completion'.on_attach{
    enable_auto_hover = 1,
    enable_auto_signature = 1
  }]]

vim.api.nvim_set_keymap('i', '<c-space>', 'completion#trigger_completion()', { noremap = true, silent = true, expr = true })

vim.g.completion_chain_complete_list = {
  {complete_items = {'lsp', 'snippet', 'path'}},
}
