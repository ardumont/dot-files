{ config, pkgs, ... }:

{
  nix.extraOptions = ''
    gc-keep-outputs = true
    gc-keep-derivations = true
  '';

  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 30d";
}
