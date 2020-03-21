{ ... }:

let path = "NetworkManager/dnsmasq.d";
in
{
  # extending networkmanager's managed dnsmasq instance
  environment.etc."${path}/cache.conf".text = ''
cache-size=1000
'';

  environment.etc."${path}/lan.conf".text = ''
server=/.lan/192.168.150.27
server=/150.168.192.in-addr.arpa/192.168.150.27
'';

  environment.etc."${path}/vlan.conf".text = ''
server=/.vlan/10.8.9.1
server=/9.8.10.in-addr.arpa/10.8.9.1
'';

  environment.etc."${path}/work.conf".text = ''
server=/internal.softwareheritage.org/192.168.100.29
server=/100.168.192.in-addr.arpa/192.168.100.29
server=/101.168.192.in-addr.arpa/192.168.100.29
server=/200.168.192.in-addr.arpa/192.168.100.29
server=/201.168.192.in-addr.arpa/192.168.100.29
server=/202.168.192.in-addr.arpa/192.168.100.29
server=/203.168.192.in-addr.arpa/192.168.100.29
server=/204.168.192.in-addr.arpa/192.168.100.29
server=/205.168.192.in-addr.arpa/192.168.100.29
server=/206.168.192.in-addr.arpa/192.168.100.29
server=/207.168.192.in-addr.arpa/192.168.100.29
'';
}
