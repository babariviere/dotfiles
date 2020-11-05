#!/usr/bin/env zsh

script="""
zinit self-update
zinit update
zinit delete --clean
"""

zsh -i -c "$script"
