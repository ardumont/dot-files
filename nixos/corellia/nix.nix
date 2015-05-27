{ config, pkgs, ... }:

{
  nix = {
    # for dev
    useChroot = true;
    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
      build-cores = 0
    '';
    maxJobs = 8;

    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
  };
}
