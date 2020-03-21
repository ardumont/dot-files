{ server, service_name, client,  device, cipher,
  with_ta, with_passfile, ... }:

let path = "/etc/openvpn/keys/${service_name}";
    optionalTa = if with_ta then "tls-auth ${path}/ta.key"else "";
    optionalPassfile = if with_passfile then "askpass ${path}/private" else "";
in {
  config = ''
remote ${server} 1194
client
dev ${device}
proto udp
comp-lzo
resolv-retry infinite
nobind
persist-key
persist-tun
mute-replay-warnings
remote-cert-tls server
key-direction 1
cipher ${cipher}
verb 1
mute 20
user nobody
group nogroup
auth-nocache
log /var/log/openvpn-${service_name}.log
status /var/log/openvpn-status-${service_name}.log
# this must be installed manually (for now)
ca ${path}/ca.crt
cert ${path}/${client}.crt
key ${path}/${client}.key
${optionalTa}
${optionalPassfile}
'';
  }
