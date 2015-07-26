{ config, pkgs, ... }:

{
  networking = {
    # hostName = "corellia";    # Define your hostname.

    firewall.enable = true;
    firewall.allowedTCPPorts = [ 22 ];
    # firewall.allowedUDPPorts = [ ];

    # nm, nmcli, etc... incompatible with networking.wireless
    # also for a user to manager wireless network, add it to networkmanager group
    networkmanager.enable = true;

    extraHosts = ''
      192.168.0.10 dagobah
      192.168.0.11 alderaan
      192.168.0.12 corellia
      192.168.0.13 job
      192.168.0.14 myrkr
      192.168.0.15 lenovo
      192.168.0.19 chris-host
      192.168.0.20 naboo
      192.168.0.28 printer-8610
      192.168.0.254 box
    '';
  };

}
