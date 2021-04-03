require "baba.globals"
require "baba.mappings"
require "baba.plugins"

require "baba.themes.dracula"
local lua_config = vim.fn.stdpath("config") .. "/lua"
function _G.fennel_compile_file(input)
  local _, e = string.find(input, "nvim/fnl/")
  local output = lua_config .. "/" .. string.sub(input, e + 1, -4) .. "lua"
  require("aniseed.compile").file(input, output)
end

-- Recompile fennel on write
vim.cmd [[autocmd BufWritePost */nvim/fnl/* lua fennel_compile_file(vim.fn.expand("%:p"))]]

-- Get highlight under cursor
vim.cmd [[
function! SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

command! SynStack :call SynStack()
]]
