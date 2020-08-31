augroup fmt
  autocmd!
  autocmd BufWritePre *.nix undojoin | Neoformat
augroup END
