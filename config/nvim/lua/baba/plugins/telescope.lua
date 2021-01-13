local telescope = require 'telescope'
local previewers = require 'telescope.previewers'

telescope.setup {
  defaults = {
    file_previewer = previewers.vim_buffer_cat.new,
    grep_previewer = previewers.vim_buffer_vimgrep.new,
    qflist_previewer = previewers.vim_buffer_qflist.new
  }
}

telescope.load_extension('fzy_native')
telescope.load_extension('gh')
