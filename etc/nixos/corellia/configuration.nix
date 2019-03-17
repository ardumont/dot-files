# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../xserver.nix
    ./openvpn.nix
    ./sound.nix
    ../laptop.nix
    ./packages.nix
    ../dns.nix
  ];
}
