{ config, pkgs, ... }:

{
  programs.ssh.startAgent = false; # do not start agent (gpg-agent will be started)

  environment.systemPackages = with pkgs; [
    feh trayer
  ];

  services.xserver.enable = true;

  # activate gpu
  hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];
}
