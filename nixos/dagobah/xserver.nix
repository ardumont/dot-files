{ config, pkgs, ... }:

{
  programs.ssh.startAgent = false; # do not start agent (gpg-agent will be started)

  services.xserver = {
    startGnuPGAgent = true;
    enable = true;

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
          haskellPackages.xmonad
          haskellPackages.xmonadContrib
          haskellPackages.xmobar
        ];
      };
    };

    # activate gpu
    vaapiDrivers = [ pkgs.vaapiIntel ];
  };

}
