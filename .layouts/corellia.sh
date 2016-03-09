#!/usr/bin/env bash
# VGA1 is the right screen
# LVDS1 is the laptop screen

width=1920
height=1080
res_screen0=${width}x${height}
res_screen1=1920x1200

screen0=eDP1

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
    screen1=$1
    res_screen1=$2
    xrandr --output $screen0 \
           --mode $res_screen0 \
           --pos ${width}x0 \
           --rotate normal \
           --output $screen1 \
           --mode $res_screen1 \
           --pos 0x0 \
           --rotate normal
}

status_connected=$(xrandr | grep -i " connected" | wc -l)

screen1=$(xrandr | grep -i " connected" | grep -v $screen0 | cut -d' ' -f1)
if [ "$screen1" = "HDMI1" ]; then
    height1=1200
else
    height1=1080
fi

[ $status_connected -eq 2 ] && two_screens $screen1 ${width}x${height1} || one_screen $screen0
