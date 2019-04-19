{ config, pkgs, ... }:

let hostName = with builtins;
  "${head (split "\n" (readFile /etc/nix-hostname))}";
in rec {
  imports = [
    # Include the specifics of the machine
    "/etc/nixos/${hostName}/configuration.nix"
    # Include the common parts
    ./common.nix
    ./nix.nix
    ./services.nix
    ./print.nix
    ./env.nix
    ./network.nix
    ./users.nix
    ./packages.nix
    ./virtual.nix
    ];

  networking.hostName = "${hostName}";
}
