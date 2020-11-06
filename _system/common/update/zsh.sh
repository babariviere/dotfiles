#!/usr/bin/env zsh

script="""
zinit self-update
zinit update
"""

zsh -i -c "$script"
