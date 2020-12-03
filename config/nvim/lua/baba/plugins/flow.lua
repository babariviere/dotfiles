function FLOW_AFTER_SWITCH(path)
  vim.api.nvim_command('edit ' .. path)
  require'telescope.builtin'.find_files {}
end

vim.api.nvim_set_keymap('n', '<leader>pp',
                        [[:lua require('flow').switch_project(FLOW_AFTER_SWITCH)<CR>]],
                        {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader>pc',
                        [[:lua require('flow').clone_project(FLOW_AFTER_SWITCH)<CR>]],
                        {noremap = true, silent = true})
