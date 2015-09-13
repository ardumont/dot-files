{ config, pkgs, ... }:

{
  nix.trustedBinaryCaches = [
    http://hydra.nixos.org
    http://cache.nixos.org
    http://hydra.nixos.org
    http://hydra.cryp.to
  ];

  # nixpkgs.config = {
  #   allowUnfree = true;

  #   firefox = {
  #     # enableAdobeFlash = true;
  #     enableGoogleTalkPlugin = true;
  #   };

  #   chromium = {
  #     enableGoogleTalkPlugin = true;
  #     enablePepperFlash = true; # Chromium's non-NSAPI alternative to Adobe Flash
  #     enablePepperPDF = true;
  #   };

  #   packageOverrides = pkgs: {
  #     # override the default pidgin with plugins (empty by default)
  #     pidgin-with-plugins = pkgs.pidgin-with-plugins.override {
  #       plugins = with pkgs; [ pidginotr skype4pidgin ];
  #     };

  #     # xmonad-with-packages = pkgs.xmonad-with-packages.override {
  #     #   packages = with pkgs.haskellngPackages.ghcWithPackages; [ xmonad-contrib xmonad-extras ];
  #     # };
  #   };
  # };

  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [ hello ];
}
