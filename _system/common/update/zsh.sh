#!/usr/bin/env zsh

script="""
if ! zplug check --verbose; then
  zplug install
fi
zplug update
zplug clean
"""

zsh -i -c "$script"
