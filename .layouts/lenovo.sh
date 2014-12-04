#!/bin/sh
# VGA1 is the right screen
# LVDS1 is the laptop screen

vga1_disconnected=`xrandr | grep -i disconnected | grep VGA1`

if [ ! -z "$vga1_disconnected" ]; then
    xrandr --output LVDS1 \
           --mode 1366x768 \
           --pos 0x0 \
           --rotate normal \
           --output VGA1 \
           --off \
           --output DP1 \
           --off \
           --output DP2 \
           --off \
           --output DP3 \
           --off \
           --output HDMI1 \
           --off \
           --output HDMI2 \
           --off \
           --output HDMI3 \
           --off
else
    xrandr --output LVDS1 \
           --mode 1366x768 \
           --pos 0x0 \
           --rotate normal \
           --output VGA1 \
           --mode 1920x1080 \
           --pos 1366x0 \
           --rotate normal \
           --output DP1 \
           --off \
           --output DP2 \
           --off \
           --output DP3 \
           --off \
           --output HDMI1 \
           --off \
           --output HDMI2 \
           --off \
           --output HDMI3 \
           --off
fi
