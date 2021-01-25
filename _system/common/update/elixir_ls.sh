#!/usr/bin/env bash

root="$HOME/src/github.com/elixir-lsp/elixir-ls"

pushd "$root" || exit
git pull
mix deps.get
MIX_ENV=prod mix compile
MIX_ENV=prod mix elixir_ls.release -o release
popd || exit
