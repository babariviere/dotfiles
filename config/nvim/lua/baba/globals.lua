vim.api.nvim_command [[syntax enable]]
vim.api.nvim_command [[filetype plugin indent on]]

local opts_info = vim.api.nvim_get_all_options_info()

local opt = setmetatable({}, {
  __newindex = function(_, key, value)
    vim.o[key] = value
    local scope = opts_info[key].scope
    if scope == "win" then
      vim.wo[key] = value
    elseif scope == "buf" then
      vim.bo[key] = value
    end
  end
})

vim.g.mapleader = " "

opt.hidden = true
opt.wrap = false
opt.encoding = "utf-8"
opt.pumheight = 10
opt.fileencoding = "utf-8"
opt.splitbelow = true
opt.splitright = true

opt.tabstop = 2
opt.softtabstop = 2
opt.shiftwidth = 2
opt.smarttab = true
opt.expandtab = true
opt.smartindent = true
opt.autoindent = true
opt.number = true
opt.cursorline = true
opt.showtabline = 2

opt.backup = false
opt.writebackup = false
opt.swapfile = true
opt.autoread = true

opt.updatetime = 200
opt.timeoutlen = 500

opt.formatoptions = "crqnj"

opt.clipboard = "unnamedplus"
opt.mouse = "a"

opt.inccommand = "nosplit"
opt.smartcase = true
opt.title = true

opt.list = true
opt.listchars = [[tab:»\ ,extends:›,precedes:‹,nbsp:·,trail:·]]
opt.termguicolors = true

opt.foldmethod = "marker"
opt.foldlevelstart = 0

opt.conceallevel = 2

opt.signcolumn = "yes:1"

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
