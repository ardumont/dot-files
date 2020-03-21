{ config, lib, ... }:

let sources = import ./niv/sources.nix;
in with {
  overlay = _: pkgs: {
    niv = import sources.niv {};    # use the sources :)
  };
};
let sources = import ./niv/sources.nix;
    pkgs = import sources.nixos {
      overlays = [ overlay ];
      inherit config;
    };
    hostName = with builtins;
      "${head (split "\n" (readFile /etc/nix-hostname))}";
in {
  imports = map (m: import m { inherit config lib pkgs; }) [
    # Include the specifics of the machine
    "/etc/nixos/${hostName}/configuration.nix"
    # Include the common parts
    ./common.nix
    ./nix.nix
    ./services.nix
    # ./print.nix
    ./env.nix
    ./network.nix
    ./users.nix
    ./packages.nix
    ./virtual.nix
  ];

  networking.hostName = "${hostName}";
}
