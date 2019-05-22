#!/bin/sh

source ~/.profile
ibus-daemon -drx

# invert caps lock and ctrl left
# setxkbmap -option "ctrl:swapcaps"

xrandr --output HDMI-2 --auto --above eDP-1

exec i3
