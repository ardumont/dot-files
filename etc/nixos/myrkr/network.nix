{ pkgs, ... }:

{
  environment.systemPackages = [ pkgs.mosh ];

  networking.firewall.allowedUDPPorts = [ 60501 ];
}
