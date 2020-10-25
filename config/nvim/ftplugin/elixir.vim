augroup ft_elixir
  au!
  au BufWritePre *.ex,*.exs undojoin | Neoformat
augroup end
