#!/bin/sh

if [ ! $(command -v rustc) ]; then
  exit 0
fi

rust_analyzer_src="$HOME/src/github.com/rust-analyzer/rust-analyzer"
if [ ! -d "$rust_analyzer_src" ]; then
  mkdir -p "$(dirname "$rust_analyzer_src")"
  git clone https://github.com/rust-analyzer/rust-analyzer.git "$rust_analyzer_src"
fi

pushd "$rust_analyzer_src" >/dev/null
cargo xtask install --server
popd
