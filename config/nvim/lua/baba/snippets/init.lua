local snips = {}
local utils = require "snippets.utils"

local function branch()
  local handle = io.popen("git branch --show-current 2>/dev/null")
  local result = handle:read("*a")
  handle:close()
  return result:gsub("^(.-)%s$", "%1")
end

snips._global = {
  todo = "TODO(babariviere): ",
  date = [[${=os.date("%Y-%m-%d")}]],
  branch = branch,
  clickup = function()
    return string.gsub(branch(), "(CU%-[0-9a-z]+).*", "%1") or ""
  end
}

snips.elixir = {
  def = utils.match_indentation [[
def $1($2) do
  $0
end]],

  defp = utils.match_indentation [[
defp $1($2) do
  $0
end
  ]],

  iex = utils.comment_and_indent [[
    iex> $0]],

  ["do"] = utils.match_indentation [[
do
  $0
end]],

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
  ["ife:"] = utils.match_indentation "if $1, do: $2, else: $0"
}

snips.go = {
  err = utils.match_indentation [[
if err != nil {
  return$0
}]]
}

snips.lua = {
  req = [[local ${2:${1|S.v:match"([^.()]+)[()]*$"}} = require '$1']],
  lambda = utils.match_indentation [[function () $0 end]]
}

local snippets = require "snippets"

snippets.snippets = snips
snippets.set_ux(require "baba.snippets.floaty")
-- snippets.use_suggested_mappings()

vim.g.completion_enable_snippet = "snippets.nvim"

return {
  expand_or_key = function(key)
    local expanded = require"snippets".expand_or_advance()
    if expanded then
      return
    end
    vim.api.nvim_eval([[feedkeys("\]] .. key .. [[", "n")]])
  end
}
