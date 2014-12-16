{ config, pkgs, ... }:

{
  # Use vbox
  services.virtualboxHost.enable = true;

  # no need because defined on the users level
  # users.extraGroups.vboxusers.members = [ "tony" ];

  # Activate docker service
  virtualisation.docker.enable = true;
}
