vim.g.qs_highlight_on_keys = {"f", "F", "t", "T"}
vim.g.qs_max_chars = 150

vim.api.nvim_command [[highlight QuickScopePrimary guifg='#00C7DF' gui=underline ctermfg=155 cterm=underline]]
vim.api.nvim_command [[highlight QuickScopeSecondary guifg='#afff5f' gui=underline ctermfg=81 cterm=underline]]
