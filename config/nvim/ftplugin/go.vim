augroup ft_go
  au!
  au BufWritePre *.go lua vim.lsp.buf.formatting_sync(nil, 1000)
augroup end
