{ config, pkgs, ... }:

let vpn-server = "ardumont.freeboxos.fr";
    client = "dagobah";
in {
  services.openvpn = import ../openvpn.nix {
    inherit vpn-server;
    inherit client;
  };
}
