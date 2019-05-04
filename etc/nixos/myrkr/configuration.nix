# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix # Include the results of the hardware scan.
    ./sound.nix
    ../mounts.nix
    ./xsession.nix
    ../dns.nix
    ./network.nix
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
