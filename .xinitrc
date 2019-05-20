#!/bin/sh

source ~/.profile
ibus-daemon -drx

# invert caps lock and ctrl left
# setxkbmap -option "ctrl:swapcaps"

exec i3
