{ config, pkgs, ... }:

{
  networking = {
    # firewall.enable = false;
    firewall.allowedTCPPorts = [ 3000 5900 ];

    hostName = "lenovo";    # Define your hostname.

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
      192.168.0.15 lenovo
      192.168.0.20 nas
    '';

    # do not use wlp3s0 names as it complicates other software use...
    usePredictableInterfaceNames = false;

    # tryout and fail
    # bridge only works with same physical devices (ethernet)
    # bridges = {
    #   br0 = { interfaces = [ "eth0" "usb0" ]; };
    # };
  };

}
