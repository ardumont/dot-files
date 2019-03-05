{ server, service_name, client, with_credential, ... }:

let path = "/etc/openvpn/keys/${service_name}";
    optional = if with_credential
               then ''
tls-auth ${path}/ta.key
askpass ${path}/private
''
               else "";
in {
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
log /var/log/openvpn-${service_name}.log
status /var/log/openvpn-status-${service_name}.log
# this must be installed manually (for now)
ca ${path}/ca.crt
cert ${path}/${client}.crt
key ${path}/${client}.key
${optional}
'';
  }
