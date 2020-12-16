#!/bin/sh

pushd /tmp 2>/dev/null

export GO111MODULE=on

go get golang.org/x/tools/gopls@latest

popd 2>/dev/null
