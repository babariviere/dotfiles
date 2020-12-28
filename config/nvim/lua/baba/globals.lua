vim.api.nvim_command [[syntax enable]]
vim.api.nvim_command [[filetype plugin indent on]]

vim.g.mapleader = ' '

local options = {
  hidden = true,
  wrap = false,
  encoding = 'utf-8',
  pumheight = 10,
  fileencoding = 'utf-8',
  splitbelow = true,
  splitright = true,
  t_Co = '256',

  tabstop = 2,
  softtabstop = 2,
  shiftwidth = 2,
  smarttab = true,
  expandtab = true,
  smartindent = true,
  autoindent = true,
  number = true,
  cursorline = true,
  showtabline = 2,

  backup = false,
  writebackup = false,
  swapfile = false,
  autoread = true,

  updatetime = 200,
  timeoutlen = 500,

  formatoptions = 'crqnj',

  clipboard = 'unnamedplus',
  mouse = 'a',

  inccommand = 'nosplit',
  smartcase = true,
  title = true,

  list = true,
  listchars = [[tab:»\ ,extends:›,precedes:‹,nbsp:·,trail:·]],
  termguicolors = true,

  foldmethod = 'marker',
  foldlevelstart = 0,

  conceallevel = 2,

  signcolumn = 'yes:1'
}

for k, v in pairs(options) do
  -- TODO: vim.o does not works well (some options are not applied)
  if v == true then
    vim.api.nvim_command('set ' .. k)
  elseif v == false then
    vim.api.nvim_command('set no' .. k)
  else
    vim.api.nvim_command('set ' .. k .. '=' .. v)
  end
end

function TRIM_TRAILING_WHITESPACE()
  local view = vim.fn.winsaveview()
  vim.api.nvim_command [[keeppatterns %substitute/\m\s\+$//e]]
  vim.fn.winrestview(view)
end

-- TODO(babariviere): convert to lua
vim.api.nvim_command [[augroup trim_spaces]]
vim.api.nvim_command [[autocmd!]]
vim.api.nvim_command [[autocmd BufWritePre * lua TRIM_TRAILING_WHITESPACE()]]
vim.api.nvim_command [[augroup END]]
