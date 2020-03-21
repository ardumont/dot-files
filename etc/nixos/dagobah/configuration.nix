{ config, lib, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix # Include the results of the hardware scan.
    ./sound.nix
    ../mounts.nix
    ../laptop.nix
    ../xsession.nix
    ../dns.nix
    ../guix.nix
    (import ../openvpn/default.nix {
      inherit config;
      inherit pkgs;
      vpn_name = "lan";
      device = "tun0";
      cipher = "AES-128-CBC";
      with_ta = true;
      with_passfile = false;
    })
    (import ../openvpn/default.nix {
      inherit config;
      inherit pkgs;
      vpn_name = "work";
      device = "tun1";
      cipher = "BF-CBC";
      with_ta = false;
      with_passfile = false;
    })
  ];
}
