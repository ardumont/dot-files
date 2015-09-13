{ config, pkgs, ... }:

{
  # Use vbox
  virtualisation.virtualbox.host.enable = false;

  # Activate docker service
  virtualisation.docker = {
    enable = true;
#    storageDriver = "devicemapper";
  };
}
