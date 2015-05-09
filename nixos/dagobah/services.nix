{config, pkgs, ...}:

{
  # List services that you want to enable:
  services = {
    acpid = {
      enable = true;     # acpi
      lidEventCommands = ''
        # suspend on lid close event or do nothing
        grep -q open /proc/acpi/button/lid/LID0/state && exit 0 || systemctl suspend
        ''; # suspend on lid close
      };

    locate = {
      enable = true;
      period = "00 19 * * *"; # update db at 19h every day
    };

    openssh.enable = true;
    ntp.enable = true;

    # https://nixos.org/wiki/Printers
    printing = {
      enable = true;
      drivers = [ pkgs.gutenprint pkgs.hplip ];
    };

    nixosManual.showManual = true; # Add the NixOS Manual on virtual console 8
  };

}
