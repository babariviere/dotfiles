augroup ft_rust
  au!
  au BufWritePre *.rs lua vim.lsp.buf.formatting_sync(nil, 1000)
  au InsertLeave,BufEnter,BufWinEnter,TabEnter *.rs :lua require'lsp_extensions'.inlay_hints{ prefix = '» ', highlight = "NonText" }
  au CursorHold,CursorHoldI *.rs :lua require'lsp_extensions'.inlay_hints{ only_current_line = true }
augroup end
