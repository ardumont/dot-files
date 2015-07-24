{ config, pkgs, ... }:

{
  services.xserver = {
    # keyboard
    layout = "fr";
    xkbOptions = "eurosign:e,terminate=ctrl_alt_backspace";

    desktopManager = {
       default = "gnome";
       kde4.enable = true;
       xfce.enable = true;
       gnome3.enable = true;
       gnome.enable = true;
     };

     displayManager.gdm.enable = true;
   };
}
