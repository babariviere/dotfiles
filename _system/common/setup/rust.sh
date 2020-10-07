#!/bin/sh

if [ ! $(command -v rustc) ]; then
  info "Installing rust"
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
  source $HOME/.cargo/env
else
  info "Rust is installed"
fi

rustup component add rust-src

rust_analyzer_src="$HOME/src/github.com/rust-analyzer/rust-analyzer"
if [ ! -d "$rust_analyzer_src" ]; then
  mkdir -p "$(dirname "$rust_analyzer_src")"
  git clone https://github.com/rust-analyzer/rust-analyzer.git "$rust_analyzer_src"
fi

pushd "$rust_analyzer_src" >/dev/null
cargo xtask install --server
popd
