{ config, pkgs, ... }:

let client = "${config.networking.hostName}";
    vpn_server_lan = "${builtins.readFile /etc/vpn-server-lan}";
    vpn_server_work = "${builtins.readFile /etc/vpn-server-work}";
in {
  environment.systemPackages = [ pkgs.openvpn ];

  services.openvpn.servers = {
    lan = import ./vpn.nix {
      inherit pkgs;
      server = vpn_server_lan;
      service_name = "lan";
      client = client;
      device = "tun0";
      cipher = "AES-128-CBC";
      with_ta = true;
      with_passfile = false;
    };

    work = import ./vpn.nix {
      inherit pkgs;
      server = vpn_server_work;
      service_name = "work";
      client = client;
      device = "tun1";
      cipher = "BF-CBC";
      with_ta = false;
      with_passfile = false;
    };
  };
}
