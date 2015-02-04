{ config, pkgs, ... }:

{
  programs.ssh = {
    startAgent = false; # do not start agent (gpg-agent will be started)
    extraConfig = ''
      Host *
      SendEnv LANG LC_*
      HashKnownHosts yes
      GSSAPIAuthentication yes
      GSSAPIDelegateCredentials no
    '';
  };
}
