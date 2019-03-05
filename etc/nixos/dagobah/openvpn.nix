{ config, pkgs, ... }:

let server = "${builtins.readFile /etc/nix-vpn-server}";
    client = "${config.networking.hostName}";
    private = false;
in {
  services.openvpn = import ../openvpn.nix {
    inherit server;
    inherit client;
    inherit private;
  };
}
