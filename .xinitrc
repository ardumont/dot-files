#!/usr/bin/env zsh

# Not supposed to be triggered in NixOS environment (already dealt with)
if [ ! -f /etc/NIXOS ]; then
    # Behavior hook which can be different depending on host
    HOOK_PER_HOST_FILE="~/.xinit/.$(hostname).init"
    # Display layout which can be different depending on host
    LAYOUT_FILE_PER_HOST="$HOME/.layouts/$(hostname).sh"

    # before starting xmonad, we want a correct X db and a running urxvt daemon
    xrdb -merge ~/.Xresources

    # fix awt & co. in stumpwm/xmonad
    export _JAVA_AWT_WM_NONREPARENTING=1
    wmname "LG3D"
    export AWT_TOOLKIT=MToolkit

    start-if-cmd-exists () {
        which $1 && ~/bin/service/service.sh restart $* &
    }

    # start default user service
    start-if-cmd-exists nemo --no-default-window
    start-if-cmd-exists xscreensaver
    start-if-cmd-exists trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --height 12 --transparent true --tint 0x000000
    start-if-cmd-exists dropbox start
    start-if-cmd-exists nm-applet

    # Layout per host divergence possible
    [ -f $LAYOUT_FILE_PER_HOST ] && zsh $LAYOUT_FILE_PER_HOST
    # Service launch per host divergence possible
    [ -f $HOOK_PER_HOST_FILE ] && . $HOOK_PER_HOST_FILE

    exec xmonad
fi
