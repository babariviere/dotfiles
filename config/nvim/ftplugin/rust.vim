if exists('g:baba_ft_rust')
  finish
endif
let g:baba_ft_rust = 1

augroup ft_rust
  au!
  au BufWritePre *.rs lua vim.lsp.buf.formatting_sync(nil, 1000)
  au CursorMoved,InsertLeave,BufEnter,BufWinEnter,TabEnter,BufWritePost *.rs silent! lua require'lsp_extensions'.inlay_hints{ prefix = '» ', highlight = "Comment", aligned = false }
augroup end
