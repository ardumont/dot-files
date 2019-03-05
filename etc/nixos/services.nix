{config, pkgs, ...}:

{
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:
  services = {
    locate = {
      enable = true;
      interval = "00 19 * * *"; # update db at 19h every day
    };
    openssh.enable = true;
    ntp.enable = true;

    nixosManual.showManual = true; # Add the NixOS Manual on virtual console 8
  };
}
