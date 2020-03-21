{ pkgs, ... }:

{
  programs.ssh.startAgent = false; # do not start agent

  services.xserver = {
    enable = true;
    exportConfiguration = true; # create link /etc/X11/xorg.conf to real conf

    # keyboard
    layout = "fr";
    xkbOptions = "eurosign:e,terminate=ctrl_alt_backspace";

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

  };

  # activate gpu
  hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];
}
