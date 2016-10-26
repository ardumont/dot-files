#!/usr/bin/env bash
# VGA1 is the right screen
# LVDS1 is the laptop screen

width=1920
height=1080
res_screen0=${width}x${height}
res_screen1=1920x1200

screen0=eDP-1

function plug_screen {
    screen0=$1
    res_screen0=$2
    xrandr --output $screen0 \
           --mode $res_screen0 \
           --pos 0x0 \
           --rotate normal
    for screen in $(xrandr | grep " disconnected" | cut -d' ' -f1); do
        xrandr --output $screen --off
    done
    echo "$screen0 plugged!"
}

function plug_screens {
    screen1=$1
    res_screen1=$2
    xrandr --output $screen1 \
           --mode $res_screen1 \
           --pos 0x0 \
           --rotate normal \
           --output $screen0 \
           --right-of $screen1 \
           --mode $res_screen0 \
           --rotate normal
    echo "$screen0 and $screen1 plugged!"
}

status_connected=$(xrandr | grep -i " connected" | wc -l)

screen1=$(xrandr | grep -i " connected" | grep -v "$screen0" | cut -d' ' -f1)
case "$screen1" in
     "HDMI-1") height1=1200;;
     "DP-1-1") height1=1200;;
esac


([ $status_connected -eq 2 ] && plug_screens $screen1 ${width}x${height1}) \
        || plug_screen $screen0 ${width}x${height}
