{ config, pkgs, ... }:

{
  networking = {
    hostName = "dagobah";    # Define your hostname.

    # firewall.allowedTCPPorts = [ ];

    # nm, nmcli, etc... incompatible with networking.wireless
    # also for a user to manager wireless network, add it to networkmanager group
    networkmanager.enable = true;
    extraHosts = ''
      192.168.0.10 dagobah
      192.168.0.11 chris-host
      192.168.0.13 job
      192.168.0.14 myrkr
      192.168.0.15 lenovo
      192.168.0.20 nas
      192.168.0.28 printer-8610
    '';
  };

}
