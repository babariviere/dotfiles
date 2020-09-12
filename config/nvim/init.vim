source $HOME/.config/nvim/general/settings.vim
source $HOME/.config/nvim/general/plugins.vim
source $HOME/.config/nvim/general/neovide.vim
source $HOME/.config/nvim/themes/one.vim
source $HOME/.config/nvim/keys/mappings.vim
source $HOME/.config/nvim/keys/which-key.vim
" source $HOME/.config/nvim/plugins/coc.vim
source $HOME/.config/nvim/plugins/codi.vim
source $HOME/.config/nvim/plugins/goyo.vim
source $HOME/.config/nvim/plugins/quickscope.vim
source $HOME/.config/nvim/plugins/neoformat.vim
source $HOME/.config/nvim/plugins/rainbow.vim
" source $HOME/.config/nvim/plugins/rnvimr.vim
source $HOME/.config/nvim/plugins/signify.vim
" source $HOME/.config/nvim/plugins/startify.vim

lua require 'plug-colorizer'
lua require 'plug-lsp'

" TODO: take a look at conceal (to have symbols)

" Reload `init.vim` on write
au! BufWritePost $MYVIMRC source %
