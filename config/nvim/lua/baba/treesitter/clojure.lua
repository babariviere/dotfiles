local function read_highlights()
  local path = vim.fn.stdpath("config") .. "/queries/clojure/highlights.scm"
  return vim.fn.join(vim.fn.readfile(path), "\n")
end

require("vim.treesitter.query").set_query("clojure", "highlights", read_highlights())
