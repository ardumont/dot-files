{ config, pkgs, ... }:

{
  imports = [ <nixos/modules/programs/virtualbox.nix> ];

  # no need because defined on the users level
  # users.extraGroups.vboxusers.members = [ "tony" ];
}
