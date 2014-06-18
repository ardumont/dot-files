#!/usr/bin/env bash

# before starting xmonad, we want a correct X db and a running urxvt daemon
xrdb -merge ~/.Xresources

# fix awt & co. in stumpwm/xmonad
export _JAVA_AWT_WM_NONREPARENTING=1
# wmname "LG3D"
# export AWT_TOOLKIT=MToolkit

# start needed service
~/bin/service/service.sh restart emacs --daemon &
~/bin/service/service.sh restart nemo --no-default-window &
~/bin/service/service.sh restart xscreensaver &
~/bin/service/service.sh restart trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --height 12 --transparent true --tint 0x000000 &
~/bin/service/service.sh restart dropbox start &
~/bin/service/service.sh restart nm-applet &
