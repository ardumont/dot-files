{ config, pkgs, ... }:

{
  # copy this template and name it proxy.nix,
  # adapt and uncomment this according to your need for proxy
  # local proxy which is cntlm
  networking.proxy = {
    default = "http://localhost:3128";
    noProxy = "127.0.0.1,localhost";
  };

  # Enterprise proxy connected with cntlm
  services.cntlm = {
    enable = true;
    username = "login";
    password = "password-in-clear"; # bad, need to improve this
    domain ="domain";
    proxy = [ "proxy1:port1" "proxy2:port2"];
    # output of: cntlm -H -u $login@$domain
    # does not work!
    # will concat this extra sample to /etc/cntlm.conf
#     extraConfig = ''
# PassLM          a
# PassNT          b
# PassNTLMv2      c
#     '';
#   };

}
