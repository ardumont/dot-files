{ config, pkgs, writeTextFile, ... }:

let path = "NetworkManager/dnsmasq.d";
in
{
  # extending networkmanager's managed dnsmasq instance
  environment.etc."${path}/cache.conf".text = ''
cache-size=1000
'';

  environment.etc."${path}/lan.conf".text = ''
server=/.lan/192.168.150.26
server=/150.168.192.in-addr.arpa/192.168.150.26
'';

  environment.etc."${path}/vlan.conf".text = ''
server=/.vlan/10.8.9.1
server=/9.8.10.in-addr.arpa/10.8.9.1
'';
}
