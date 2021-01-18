local bindings = {
  i = {
    ["<C-j>"] = {
      [[pumvisible() ? "\<C-n>" : "\<cmd>lua require('snippets').expand_or_advance()<CR>"]],
      expr = true,
      noremap = true
    },
    ["<C-k>"] = {
      [[pumvisible() ? "\<C-p>" : "\<cmd>lua require('snippets').advance_snippet(-1)<CR>"]],
      expr = true,
      noremap = true
    },
    ["<Tab>"] = {[[<cmd>lua require'baba.snippets'.expand_or_key("<tab>")<CR>]], noremap = true},
    ["<CR>"] = {[[pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"]], expr = true, noremap = true},
    ["<up>"] = {[[pumvisible() ? '<c-e><up>' : '<up>']], expr = true, noremap = true},
    ["<down>"] = {[[pumvisible() ? '<c-e><down>' : '<down>']], expr = true, noremap = true}
  },

  n = {
    ["<TAB>"] = {":bnext<CR>", noremap = true},
    ["<S-TAB>"] = {":bprevious<CR>", noremap = true},
    ["<C-h>"] = {"<C-w>h", noremap = true},
    ["<C-j>"] = {"<C-w>j", noremap = true},
    ["<C-k>"] = {"<C-w>k", noremap = true},
    ["<C-l>"] = {"<C-w>l", noremap = true},
    ["<leader><space>"] = {[[<cmd>lua require'telescope.builtin'.find_files{}<CR>]], noremap = true},
    ["<leader>,"] = {[[<cmd>lua require'telescope.builtin'.buffers{}<CR>]], noremap = true},
    ["<leader>;"] = {[[<cmd>lua require'telescope.builtin'.commands{}<CR>]], noremap = true},
    ["<leader>bd"] = {[[:bdelete<CR>]], noremap = true},
    ["<leader>sb"] = {[[<cmd>lua require'telescope.builtin'.buffers{}<CR>]], noremap = true},
    ["<leader>sp"] = {[[<cmd>lua require'telescope.builtin'.live_grep{}<CR>]], noremap = true}
  },

  v = {["<"] = {"<gv", noremap = true}, [">"] = {">gv", noremap = true}}
}

for map, set in pairs(bindings) do
  for key, opts in pairs(set) do
    local cmd = table.remove(opts, 1)
    vim.api.nvim_set_keymap(map, key, cmd, opts)
  end
end

vim.cmd [[
func Eatchar(pat)
  let c = nr2char(getchar(0))
  return (c =~ a:pat) ? '' : c
endfunc
]]
vim.cmd [[ cnoreabbrev <expr> e getcmdtype() == ":" && getcmdline() == 'e' ? 'e <C-R>=expand("%:h") . "/"<CR><C-R>=Eatchar(" ")<CR>' : 'e' ]]
