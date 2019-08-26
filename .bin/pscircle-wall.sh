#!/bin/sh

pscircle \
    --output=/tmp/wallpaper.png \
    --output-width=1920 \
    --output-height=1080 \
    --tree-font-size=12 \
    --dot-radius=3 \
    --max-children=35 \
    --toplists-font-size=10 \
    --tree-rotate=true \
    --tree-rotation-angle=3.5342 \
    --tree-center=0:150 \
    --tree-sector-angle=3.9269 \
    --tree-radius-increment=200 \
    --cpulist-center=-250:400 \
    --memlist-center=250:400 \
    --root-pid=1

feh --bg-center /tmp/wallpaper.png
