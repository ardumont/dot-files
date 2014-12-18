{ config, pkgs, ... }:

{
  services.virtualboxHost.enable = true;
  services.virtualboxHost.enableHardening = true;

  # no need because defined on the users level
  # users.extraGroups.vboxusers.members = [ "tony" ];

  # Activate docker service
  virtualisation.docker.enable = true;
}
