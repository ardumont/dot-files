{ server, client, private, ... }:

let network = "lan";
    path = "/etc/openvpn/keys/${network}";
    optional = if private
               then "askpass ${path}/private"
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
log /var/log/openvpn-${network}.log
${optional}
status /var/log/openvpn-status-${network}.log
# this must be installed manually (for now)
ca ${path}/ca.crt
cert ${path}/${client}.crt
key ${path}/${client}.key
tls-auth ${path}/ta.key
      '';
      autoStart = true;
    };
  };
}
