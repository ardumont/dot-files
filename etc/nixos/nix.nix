{ config, pkgs, ... }:

{
  nix = {
    trustedBinaryCaches = [
      https://hydra.nixos.org
      https://cache.nixos.org
    ];

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
