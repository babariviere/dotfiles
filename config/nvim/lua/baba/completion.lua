vim.api.nvim_command [[autocmd BufEnter * lua require'completion'.on_attach{
    enable_auto_hover = 1,
    enable_auto_signature = 1
  }]]

vim.api.nvim_set_keymap('i', '<c-space>', 'completion#trigger_completion()',
                        {noremap = true, silent = true, expr = true})

-- TODO(babariviere): only allow path in string or comments
vim.g.completion_chain_complete_list = {
  {complete_items = {'lsp', 'snippets'}},
  {complete_items = {'path'}},
  {mode = '<c-p>'},
  {mode = '<c-n>'}
}
