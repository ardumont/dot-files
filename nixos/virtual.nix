{ config, pkgs, ... }:

{
  # Use vbox
  virtualisation.virtualbox.host.enable = true;

  # Activate docker service
  virtualisation.docker = {
    enable = true;
#    storageDriver = "devicemapper";
  };
}
