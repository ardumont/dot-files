{ config, pkgs, vpn_name, with_ta, with_passfile, device, cipher, ... }:

let client = "${config.networking.hostName}";
    path = "/etc/vpn-server-${vpn_name}";
    read_from_file = with builtins; path: head (split "\n" (readFile path));
    vpn_server = "${read_from_file path}";
in {
  environment.systemPackages = [ pkgs.openvpn ];

  services.openvpn.servers = {
    "${vpn_name}" = import ./vpn.nix {
      inherit pkgs;
      server = "${vpn_server}";
      service_name = "${vpn_name}";
      inherit client;
      inherit device;
      inherit cipher;
      inherit with_ta;
      inherit with_passfile;
    };
  };
}
