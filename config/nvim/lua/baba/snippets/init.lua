local snips = {}
local utils = require 'snippets.utils'

snips._global = {
  todo = "TODO(babariviere): ",
  date = [[${=os.date("%Y-%m-%d")}]],
}

snips.go = {
  err = utils.match_indentation [[
  if err != nil {
    return$0
  }]]
}

snips.lua = {
  req = [[local ${2:${1|S.v:match"([^.()]+)[()]*$"}} = require '$1']]
}

local snippets = require 'snippets'

snippets.snippets = snips
snippets.set_ux(require'baba.snippets.floaty')
-- snippets.use_suggested_mappings()

vim.g.completion_enable_snippet = "snippets.nvim"
