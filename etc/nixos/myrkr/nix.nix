{ pkgs, ... }:

{
  nix = {
    extraOptions = ''
      secret-key-files = /etc/nix/myrkr-signing-key.sec
    '';
  };
}
