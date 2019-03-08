{ config, pkgs, ... }:

{
  nix = {
    # for dev
    useSandbox = true;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      keep-env-derivations = true
      cores = 4
    '';

    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
  };
}
