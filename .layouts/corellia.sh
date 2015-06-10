#!/usr/bin/env bash
# VGA1 is the right screen
# LVDS1 is the laptop screen

width=1920
height=1080
res_screen0=${width}x${height}
res_screen1=1600x1200

screen0=eDP1
screen1=DP2

function one_screen {
    echo "one screen"
    xrandr --output $screen0 \
           --mode $res_screen0 \
           --pos 0x0 \
           --rotate normal \
           --output $screen1 \
           --off
}

function two_screens {
    echo "two screens"
    xrandr --output $screen0 \
           --mode $res_screen0 \
           --pos 0x0 \
           --rotate normal \
           --output $screen1 \
           --mode $res_screen1 \
           --pos ${width}x0 \
           --rotate normal
}

set -x

status_disconnected=$(xrandr | grep -i disconnected | grep -i $screen1)

if [ ! -z "$status_disconnected" ]; then
    one_screen
else
    two_screens
fi
