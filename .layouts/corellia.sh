#!/bin/sh
# VGA1 is the right screen
# LVDS1 is the laptop screen

disconnect=$1

width=1920
height=1080
res_screen0=${width}x${height}
res_screen1=1600x1200

screen0=eDP1
screen1=DP2

if [ ! -z "$disconnect" ]; then
    xrandr --output $screen0 \
           --mode $res_screen0 \
           --pos 0x0 \
           --rotate normal \
           --output $screen1 \
           --off
else
    xrandr --output $screen0 \
           --mode $res_screen0 \
           --pos 0x0 \
           --rotate normal \
           --output $screen1 \
           --mode $res_screen1 \
           --pos ${width}x0 \
           --rotate normal
fi
