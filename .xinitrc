#!/usr/bin/env zsh

# before starting xmonad, we want a correct X db and a running urxvt daemon
xrdb -merge ~/.Xresources

# fix awt & co. in stumpwm/xmonad
export _JAVA_AWT_WM_NONREPARENTING=1
# wmname "LG3D"
# export AWT_TOOLKIT=MToolkit

LAYOUT_FILE="$HOME/.layouts/$(hostname).sh"
[ -d ~/.layouts -a -f $LAYOUT_FILE ] && zsh $LAYOUT_FILE

start-if-cmd-exists () {
  which $1 && ~/bin/service/service.sh restart $* &
}

# start needed service
start-if-cmd-exists nemo --no-default-window
start-if-cmd-exists xscreensaver
start-if-cmd-exists trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --height 12 --transparent true --tint 0x000000
start-if-cmd-exists dropbox start
start-if-cmd-exists nm-applet

[ -f /etc/NIXOS ] && exec xmonad # Run xmonad on NixOS
