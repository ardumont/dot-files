{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    firefox libreoffice pinta pass gnupg git
  ];
}
