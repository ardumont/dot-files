{ config, pkgs, ... }:

{
  programs.ssh.startAgent = false; # do not start agent

  services.xserver = {
    enable = true;
    exportConfiguration = true; # create link /etc/X11/xorg.conf to real conf

    # keyboard
    layout = "us";
    xkbOptions = "eurosign:e,terminate=ctrl_alt_backspace";

    desktopManager = {
       default = "none";
       plasma5.enable = true;
       gnome3.enable = true;
       mate.enable = true;
    };

    displayManager.lightdm.enable = true;
  };
}
