require("neogit").setup {}

vim.api.nvim_set_keymap("n", "<leader>gg", ":Neogit<CR>", {noremap = true, silent = true})
