{ config, pkgs, ... }:

{
  programs.ssh.startAgent = false; # do not start agent

  services.xserver = {
    enable = true;
    exportConfiguration = true; # create link /etc/X11/xorg.conf to real conf

    # keyboard
    layout = "fr";
    xkbOptions = "eurosign:e,terminate=ctrl_alt_backspace";

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
       default = "gnome3";
       kde4.enable = true;
       xfce.enable = true;
       gnome3.enable = true;
     };

     # displayManager.gdm.enable = true;
     # displayManager.kdm.enable = true;
     displayManager.lightdm.enable = true;
     # displayManager.dm.enable = true;

    # activate gpu
    vaapiDrivers = [ pkgs.vaapiIntel ];
  };

}
