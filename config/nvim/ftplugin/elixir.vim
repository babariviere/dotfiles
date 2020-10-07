augroup elixir
  au!
  au BufWritePre *.ex,*.exs undojoin | Neoformat
augroup end
