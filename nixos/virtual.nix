{ config, pkgs, ... }:

{
  # Use vbox
  services.virtualboxHost.enable = false;

  # Activate docker service
  # virtualisation.docker.enable = true;
}
