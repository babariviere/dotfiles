--- DAP {{{
local dap = require("dap")
dap.adapters.elixir = {
  type = "executable",
  command = os.getenv("HOME") .. "/src/github.com/elixir-lsp/elixir-ls/release/debugger.sh",
  args = {}
}
dap.configurations.elixir = {
  {
    type = "elixir",
    request = "launch",
    name = "mix test",
    task = "test",
    taskArgs = {"--trace"},
    projectDir = "${workspaceFolder}",
    requireFiles = {"test/**/test_helper.exs", "test/**/*_test.exs"}
  }
}
--- }}}

--- Virtual Text {{{
vim.g.dap_virtual_text = true
--- }}}
