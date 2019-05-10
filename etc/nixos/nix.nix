{ config, pkgs, ... }:

{
  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      keep-env-derivations = true
    '';

    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
  };
}
