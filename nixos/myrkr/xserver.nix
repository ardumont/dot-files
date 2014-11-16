{ config, pkgs, ... }:

{
  services.xserver = {
      enable = true;
      videoDrivers = [ "ati_unfree" ];

      startGnuPGAgent = true;
      desktopManager.default = "none";
      layout = "us";
      xkbOptions = "eurosign:e,ctrl:nocaps,terminate=ctrl_alt_backspace,altwin:meta_alt";

      # touchpad
#      synaptics.enable = true;
#      synaptics.twoFingerScroll = true;

      windowManager = {
        default = "xmonad";
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = self: [
            self.xmonad
            self.xmonadContrib
            self.xmobar
          ];
        };
      };

      # Enable GPU support (source: https://nixos.org/wiki/Enable_Browser_Plugins)
      vaapiDrivers = [ pkgs.vaapiIntel ];
    };
}
