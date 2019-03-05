{ config, pkgs, ... }:

{
  virtualisation = {
    # No vbox
    virtualbox.host.enable = false;
    # Activate docker service
    docker = {
      enable = true;
      # storageDriver = "devicemapper";
    };
  };
}
