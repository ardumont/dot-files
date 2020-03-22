{ ... }:

{
  # List services that you want to enable:
  services = {
    locate = {
      enable = true;
      interval = "00 19 * * *"; # update db at 19h every day
    };
    openssh = {
      enable = true;
    extraConfig = ''
# To allow ~/.ssh/authorized_keys to be linked on /nix/store (world
# readable) on clients...
StrictModes no
      '';
    };
    ntp.enable = true;

    nixosManual.showManual = true; # Add the NixOS Manual on virtual console 8
  };
}
