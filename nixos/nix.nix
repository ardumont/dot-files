{ config, pkgs, ... }:

{
  nix = {
    # for dev
    useSandbox = true;
    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
      build-cores = 0
    '';

    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
  };
}
