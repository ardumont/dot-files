{ config, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.mosh ];

  networking.firewall = {
    enable = true;
    allowedUDPPorts = [ 60501 ];
  };
}
