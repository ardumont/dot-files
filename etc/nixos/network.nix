{ config, pkgs, ... }:

{
  networking = {
    firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];
    };

    # nm, nmcli, etc... incompatible with networking.wireless
    # also for a user to manager wireless network, add it to
    # networkmanager group
    networkmanager = {
      enable = true;
      dns = "dnsmasq";
    };
  };
}
