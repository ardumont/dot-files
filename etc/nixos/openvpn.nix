{ server, client, stuff, ... }:

let optional = if stuff
               then "askpass /etc/openvpn/keys/stuff"
               else "";
in {
  servers = {
    lan = {
      config = ''
remote ${server} 1194
client
dev tun0
proto udp
comp-lzo
resolv-retry infinite
nobind
persist-key
persist-tun
mute-replay-warnings
remote-cert-tls server
key-direction 1
cipher AES-128-CBC
verb 1
mute 20
user nobody
group nogroup
log /var/log/openvpn-lan.log
${optional}
status /var/log/openvpn-status-lan.log
# this must be installed manually (for now)
ca /etc/openvpn/keys/lan/ca.crt
cert /etc/openvpn/keys/lan/${client}.crt
key /etc/openvpn/keys/lan/${client}.key
tls-auth /etc/openvpn/keys/lan/ta.key
      '';
      autoStart = true;
    };
  };
}
