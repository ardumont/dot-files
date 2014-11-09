{ config, pkgs, ... }:

# source: https://nixos.org/wiki/Installing_VirtualBox_on_NixOS
{
  imports = [ <nixos/modules/programs/virtualbox.nix> ];

  # no need to use this because the users.nix already define tony to be a member of it
# users.extraGroups.vboxusers.members = [ "tony" ];
}
