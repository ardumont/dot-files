{ config, pkgs, ... }:

{
  networking = {
    firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];
    };

    # nm, nmcli, etc... incompatible with networking.wireless
    # also for a user to manager wireless network, add it to networkmanager group
    networkmanager.enable = true;

    extraHosts = ''
      192.168.150.10 dagobah
      192.168.150.11 alderaan
      192.168.150.12 corellia
      192.168.150.13 job
      192.168.150.14 myrkr
      192.168.150.15 pi1
      192.168.150.19 chris-host
      192.168.150.26 rpi3
      192.168.150.28 printer-8610
      192.168.150.254 box
    '';
  };

}
