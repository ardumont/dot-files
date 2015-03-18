{ config, pkgs, ... }:

{
  services.openssh.enable = false;
  programs.ssh = {
    startAgent = false; # do not start agent (gpg-agent will be started)
    extraConfig = ''
       Ciphers 3des-cbc,blowfish-cbc,cast128-cbc,arcfour,arcfour128,arcfour256,aes128-cbc,aes192-cbc,aes256-cbc,rijndael-cbc@lysator.liu.se,aes128-ctr,aes192-ctr,aes256-ctr,aes128-gcm@openssh.com,aes256-gcm@openssh.com,chacha20-poly1305@openssh.com
       UseRoaming No
    '';
  };
}
