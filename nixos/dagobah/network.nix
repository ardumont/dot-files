{ config, pkgs, ... }:

{
  # nix.proxy = "http://login:mdp@proxy:port";
  networking = {
    hostName = "dagobah";    # Define your hostname.

    # wireless = {
    #   enable = true;                # Enables wireless
    #   userControlled.enable = true; # user can play with wifi
    #   interfaces = [ "wlp1s0" ];    # explicit the interfaces the user can modify
    # };

    networkmanager.enable = true; # nm, nmcli, etc... incompatible with networking.wireless
    extraHosts = ''
      192.168.0.10 dagobah
      192.168.0.11 chris-host
      192.168.0.13 job
      192.168.0.14 myrkr
      192.168.0.20 nas
    '';
  };

}