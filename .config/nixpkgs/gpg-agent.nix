{ pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1728000;
    defaultCacheTtlSsh = 1728000;
    maxCacheTtl = 1728000;
    maxCacheTtlSsh = 1728000;

    extraConfig = ''
      pinentry-program ${pkgs.pinentry}/bin/pinentry-tty
    '';

  };
}