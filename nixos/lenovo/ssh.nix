{ config, pkgs, ... }:

{
  programs.ssh = {
    startAgent = false; # do not start agent (gpg-agent will be started)
  };
}
