local bindings = {
  n = {["<TAB>"] = {":BufferNext<CR>", noremap = true}, ["<S-TAB>"] = {":BufferPrevious<CR>", noremap = true}}
}

for map, set in pairs(bindings) do
  for key, opts in pairs(set) do
    local cmd = table.remove(opts, 1)
    vim.api.nvim_set_keymap(map, key, cmd, opts)
  end
end
