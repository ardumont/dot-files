{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./sound.nix
    ../mounts.nix
    ./xsession.nix
    ../dns.nix
    ./network.nix
    ./nix.nix
    ./packages.nix
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
