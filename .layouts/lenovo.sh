#!/bin/sh
xrandr --output VIRTUAL1 \
       --off \
       --output LVDS1 \
       --mode 1366x768 \
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
       --off \
       --output VGA1 \
       --mode 1280x1024 \
       --pos 0x0 \
       --rotate normal
