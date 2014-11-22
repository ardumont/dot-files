{ config, pkgs, ... }:

{
  # nix.proxy = "http://login:mdp@proxy:port";
  # environment.variables = {
  #   no_proxy = "127.0.0.1,localhost";
  # };

  networking = {
    hostName = "dagobah";    # Define your hostname.

    # wireless = {
    #   enable = true;                # Enables wireless
    #   userControlled.enable = true; # user can play with wifi
    #   interfaces = [ "wlp1s0" ];    # explicit the interfaces the user can modify
    # };

    # nm, nmcli, etc... incompatible with networking.wireless
    # also for a user to manager wireless network, add it to networkmanager group
    networkmanager.enable = true;
    extraHosts = ''
      192.168.0.10 dagobah
      192.168.0.11 chris-host
      192.168.0.13 job
      192.168.0.14 myrkr
      192.168.0.20 nas
    '';
  };

}
