#!/bin/sh

nvim --headless +PlugUpgrade +qall
nvim --headless +'PlugUpdate --sync' +qall
