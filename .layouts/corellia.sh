#!/usr/bin/env bash
# VGA1 is the right screen
# LVDS1 is the laptop screen

width=1920
height=1080
res_screen0=${width}x${height}
res_screen1=1920x1200

screen0=eDP1
screen1=HDMI1

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

status_connected=$(xrandr | grep -i " connected" | grep -i $screen1)

[ ! -z "$status_connected" ] && two_screens || one_screen
