{ config, pkgs, ... }:

# source: https://nixos.org/wiki/Installing_VirtualBox_on_NixOS
{
  services.virtualboxHost.enable = true;

  # no need to use this because the users.nix already define tony to be a member of it
# users.extraGroups.vboxusers.members = [ "tony" ];

  # Activate docker service
  virtualisation.docker.enable = true;
}
