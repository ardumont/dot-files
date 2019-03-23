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
      with_credential = true;
      device = "tun0";
      cipher = "AES-128-CBC";
    };

    work = import ./vpn.nix {
      inherit pkgs;
      server = vpn_server_work;
      service_name = "work";
      client = client;
      with_credential = false;
      device = "tun1";
      cipher = "BF-CBC";
    };
  };
}
