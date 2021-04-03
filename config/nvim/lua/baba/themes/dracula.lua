vim.o.background = "dark"

vim.g.lightline = {colorschem = "dracula"}
vim.g.airline_theme = "dracula"

vim.api.nvim_command [[colorscheme dracula]]

-- vim.api.nvim_command [[highlight DraculaDiffDelete guibg=none ctermbg=none]]
vim.api.nvim_command [[highlight BufferCurrentMod guifg=#f8f8f2 guibg=#282a36]]
