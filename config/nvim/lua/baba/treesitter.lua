-- local parser_config = require"nvim-treesitter.parsers".get_parser_configs()
-- parser_config.markdown = {
--   install_info = {url = "https://github.com/ikatyang/tree-sitter-markdown", files = {"src/parser.c", "src/scanner.cc"}}
-- }
require"nvim-treesitter.configs".setup {
  ensure_installed = "maintained", -- one of "all", "language", or a list of languages
  highlight = {
    enable = true, -- false will disable the whole extension
    disable = {}
    -- disable = { "c", "rust" },  -- list of language that will be disabled
  },
  indent = {enable = true},
  query_linter = {enable = true, use_virtual_text = true, lint_events = {"BufWrite", "CursorHold"}}
}
