#!/usr/bin/env zsh

# Not supposed to be triggered in NixOS environment (already dealt with)
if [ ! -f /etc/NIXOS ]; then
    # Display layout which can be different depending on host
    LAYOUT_FILE_PER_HOST="$HOME/.layouts/$(hostname).sh"

    # before starting xmonad, we want a correct X db and a running urxvt daemon
    xrdb -merge ~/.Xresources

    # fix graphical libraries (awt, swt, etc...) in stumpwm/xmonad (exotic wm)
    export _JAVA_AWT_WM_NONREPARENTING=1
    wmname "LG3D"
    export AWT_TOOLKIT=MToolkit

    /usr/lib/notification-daemon/notification-daemon&

    start-if-cmd-exists () {
        which $1 && ~/bin/service/service.sh restart $* &
    }

    # start default user service
    systemctl --user start emacs.service
    systemctl --user start xscreensaver.service
    systemctl --user start offlineimap.timer

    start-if-cmd-exists restart /usr/lib/notification-daemon/notification-daemon
    start-if-cmd-exists trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --height 12 --transparent true --tint 0x000000
    start-if-cmd-exists nm-applet

    # Layout per host divergence possible
    [ -f $LAYOUT_FILE_PER_HOST ] && zsh $LAYOUT_FILE_PER_HOST

    exec xmonad
fi
