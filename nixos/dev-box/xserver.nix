{ config, pkgs, ... }:

{
  programs.ssh.startAgent = false; # do not start agent (gpg-agent will be started)

  services.xserver = {
    enable = true;
    exportConfiguration = true; # create link /etc/X11/xorg.conf to real conf

    # keyboard
    layout = "us";
    xkbOptions = "eurosign:e,ctrl:nocaps,terminate=ctrl_alt_backspace,altwin:meta_alt";

    # touchpad
    synaptics = {
      enable = true;
      twoFingerScroll = true;

      # - Activate palm detection
      palmDetect = false;
      # - Activate tap to click
      tapButtons = true;
      # - Activate 2 fingers tapping as right click
      # - Activate 3 fingers tapping as middle click
      buttonsMap = [ 1 3 2 ];

      # additionalOptions = ''
      #   Option "PalmDetect" "0"
      #   Option "TapButton1" "1"
      #   Option "TapButton2" "3"
      #   Option "TapButton3" "2"
      # '';
    };

    desktopManager = {
      default = "none";
    };

    windowManager = {
      default = "xmonad";
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmobar
        ];
      };
    };

    displayManager.sessionCommands = with pkgs; ''
      # before starting xmonad, we want a correct X db and a running urxvt daemon
      ${xlibs.xrdb}/bin/xrdb -merge ~/.Xresources
      ${xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
      # set the keyboard repeat rate
      ${xlibs.xset}/bin/xset r rate 200 60
      # start default user service
      ${xscreensaver}/bin/xscreensaver&
      ${trayer}/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --height 12 --transparent true --tint 0x000000&
      ${networkmanagerapplet}/bin/nm-applet&
      ${feh}/bin/feh --bg-max ~/.wallpaper.jpg

      export BROWSER="firefox";

      # fix awt & co. in stumpwm/xmonad
      export _JAVA_AWT_WM_NONREPARENTING=1
      ${wmname}/bin/wmname "LG3D"
      export AWT_TOOLKIT=MToolkit

      # Display layout which can be different depending on host
      LAYOUT_FILE_PER_HOST="$HOME/.layouts/$(hostname).sh"

      # Layout per host divergence possible
      [ -f $LAYOUT_FILE_PER_HOST ] && ${zsh}/bin/zsh $LAYOUT_FILE_PER_HOST

      exec ${haskellPackages.xmonad}/bin/xmonad
    '';

  };

  # activate gpu
  hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];
}
