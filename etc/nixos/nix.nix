{ config, pkgs, ... }:

{
  nix = {
    # for dev
    useSandbox = true;
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
