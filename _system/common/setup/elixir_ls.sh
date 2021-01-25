#!/usr/bin/env bash

root="$HOME/src/github.com/elixir-lsp/elixir-ls"
bin="$root/release/language_server.sh"

if [ ! -d "$root" ]; then
  mkdir -p "$(dirname $root)"
  git clone https://github.com/elixir-lsp/elixir-ls "$root"
fi

if [ ! -f "$bin" ]; then
  pushd "$root" || exit
  mix deps.get
  MIX_ENV=prod mix compile
  MIX_ENV=prod mix elixir_ls.release -o release
  popd || exit
fi
