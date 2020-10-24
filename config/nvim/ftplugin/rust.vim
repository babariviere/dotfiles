augroup rust
  au!
  au BufWritePre *.rs lua vim.lsp.buf.formatting_sync(nil, 1000)
augroup end
