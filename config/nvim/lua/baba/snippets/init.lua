local snips = {}
local utils = require "snippets.utils"

local function branch()
  local handle = io.popen("git branch --show-current 2>/dev/null")
  local result = handle:read("*a")
  handle:close()
  return result:gsub("^(.-)%s$", "%1")
end

local function parse_clickup_ticket(s)
  return string.gsub(s, "(CU%-[0-9a-z]+).*", "%1")
end

snips._global = {
  todo = "TODO(babariviere): ",
  date = [[${=os.date("%Y-%m-%d")}]],
  branch = branch,
  clickup = function()
    return parse_clickup_ticket(branch()) or ""
  end,
  clickupr = function()
    return "[" .. parse_clickup_ticket(branch()) .. "]"
  end
}

snips.elixir = {
  df = utils.match_indentation [[
def $1($2) do
  $0
end]],

  dfp = utils.match_indentation [[
defp $1($2) do
  $0
end
  ]],

  ["df:"] = "def $1($2), do: $0",
  ["dfp:"] = "defp $1($2), do: $0",

  iex = utils.comment_and_indent [[
    iex> $0]],

  ["do"] = utils.match_indentation [[
do
  $0
end]],

  ["do:"] = "do: $0",

  ["fn"] = "fn ${1:args} -> $0 end",

  ["case"] = utils.match_indentation [[
case $1 do
  $2 ->
    $0
end]],

  ["if"] = utils.match_indentation [[
if $1 do
  $0
end]],
  ["if:"] = utils.match_indentation "if $1, do: $0",
  ["ife:"] = utils.match_indentation "if $1, do: $2, else: $0",

  ["p"] = "|> $0",
  [">e"] = "|> Enum.each(fn $1 -> $0 end)",
  [">i"] = "|> IO.inspect()$0",

  ["put"] = "IO.puts($1)$0",
  ["ins"] = "IO.inspect($1)$0",

  ["doc"] = utils.match_indentation [[
@doc """
$0
"""
]]
}

snips.go = {
  err = utils.match_indentation [[
if err != nil {
  return$0
}]]
}

snips.lua = {
  req = [[local ${2:${1|S.v:match"([^.()]+)[()]*$"}} = require '$1']],
  lambda = utils.match_indentation [[function () $0 end]],
  t = [[
  local
  ${2:hello} = require '$1'
  $0]]
}

local snippets = require "snippets"

snippets.snippets = snips
-- snippets.set_ux(require "baba.snippets.floaty")
snippets.set_ux(require "baba.snippets.select")
-- snippets.set_ux(require "snippets.inserters.vim_input")
-- snippets.use_suggested_mappings()

vim.g.completion_enable_snippet = "snippets.nvim"

return {
  expand_or_key = function(key)
    if snippets.advance_snippet(1) then
      return
    end
    if snippets.expand_at_cursor() then
      return
    end
    vim.fn.feedkeys(key, "n")
  end
}
