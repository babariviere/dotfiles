#!/bin/sh

pushd /tmp

export GO111MODULE=on

go get golang.org/x/tools/gopls@latest

popd /tmp
