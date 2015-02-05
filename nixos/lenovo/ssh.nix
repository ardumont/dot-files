{ config, pkgs, ... }:

{
  services.openssh.enable = false;
  programs.ssh = {
    startAgent = false; # do not start agent (gpg-agent will be started)
  };
}
