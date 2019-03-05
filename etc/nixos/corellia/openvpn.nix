{ config, pkgs, ... }:

let server = "${builtins.readFile /etc/nix-vpn-server}";
    client = "${config.networking.hostName}";
    stuff = true;
in {
  services.openvpn = import ../openvpn.nix {
    inherit server;
    inherit client;
    inherit stuff;
  };
}
