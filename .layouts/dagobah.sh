#!/bin/sh
# VGA1 is the right screen
# LVDS1 is the laptop screen

disconnect=$1

if [ ! -z "$disconnect" ]; then
    xrandr --output eDP1 \
           --mode 1920x1080 \
           --pos 0x0 \
           --rotate normal \
           --output HDMI1 \
           --off
else
    xrandr --output eDP1 \
           --mode 1920x1080 \
           --pos 0x0 \
           --rotate normal \
           --output HDMI1 \
           --mode 1280x720 \
           --pos 1920x0 \
           --rotate normal
fi
