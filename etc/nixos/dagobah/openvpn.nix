{ config, pkgs, ... }:

let vpn-server = "dagobah";
in {
  services.openvpn = import ../openvpn.nix { inherit vpn-server; };
}
