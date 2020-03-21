{ config, lib, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../xserver.nix
    ./sound.nix
    ../laptop.nix
    ./packages.nix
    ../dns.nix
    ../xsession.nix
    (import ../openvpn/default.nix {
      inherit config;
      inherit pkgs;
      vpn_name = "lan";
      device = "tun0";
      cipher = "AES-128-CBC";
      with_ta = true;
      with_passfile = true;
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
