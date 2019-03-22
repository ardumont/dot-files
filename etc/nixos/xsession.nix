{ config, pkgs, ... }:

{
  # basic xscreenserver for the login manager
  services.xserver = {
    enable = true;
    exportConfiguration = true;
    desktopManager = {
      default = "none";
    };
    displayManager.lightdm.enable = true;
  };
}