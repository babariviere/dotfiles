vim.g.lightline = vim.tbl_extend('force', vim.g.lightline, {
  active = {
    left = {
      {'mode', 'paste'}, {'gitbranch', 'readonly', 'filename', 'modified'}
    },
    right = {
      {'lineinfo'}, {'percent'},
      {'fileformat', 'fileencoding', 'filetype', 'lspstatus'}
    }
  },
  tabline = {left = {{'buffers'}}, right = {{}}},
  component_function = {gitbranch = 'FugitiveHead', lspstatus = 'LspStatus'},
  component_expand = {buffers = 'lightline#bufferline#buffers'},
  component_type = {buffers = 'tabsel'},
  tabline_separator = {left = '', right = ''},
  tabline_subseparator = {left = '', right = ''}
})
