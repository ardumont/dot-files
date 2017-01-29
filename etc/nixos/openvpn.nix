{ vpn-server, ... }:

{
  servers = {
    lan = {
      config = ''
        remote rpi3.lan 1194
        client
        dev tun0
        proto udp
        comp-lzo
        resolv-retry infinite
        nobind
        persist-key
        persist-tun
        mute-replay-warnings
        ns-cert-type server
        key-direction 1
        cipher AES-128-CBC
        verb 1
        mute 20
        user nobody
        group nogroup
        log /var/log/openvpn-${vpn-server}.log
        status /var/log/openvpn-status-${vpn-server}.log
        # this must be installed manually
        ca /etc/openvpn/keys/lan/ca.crt
        cert /etc/openvpn/keys/lan/${vpn-server}.crt
        key /etc/openvpn/keys/lan/${vpn-server}.key
        tls-auth /etc/openvpn/keys/lan/ta.key
      '';
      autoStart = false;
    };
  };
}
