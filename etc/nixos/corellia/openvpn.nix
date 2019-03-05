{ config, pkgs, ... }:

let client = "${config.networking.hostName}";
    vpn_server_lan = "${builtins.readFile /etc/vpn-server-lan}";
in {
  services.openvpn = import ../openvpn.nix {
    server = vpn_server_lan;
    client = client;
    with_credential = true;
  };
}
