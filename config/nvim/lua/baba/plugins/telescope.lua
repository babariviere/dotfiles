local telescope = require "telescope"
local themes = require "telescope.themes"
local previewers = require "telescope.previewers"

telescope.setup {
  defaults = themes.get_dropdown {
    file_previewer = previewers.vim_buffer_cat.new,
    grep_previewer = previewers.vim_buffer_vimgrep.new,
    qflist_previewer = previewers.vim_buffer_qflist.new
  }
}

telescope.load_extension("gh")
