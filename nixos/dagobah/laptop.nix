{ config, pkgs, ... }:

{
  powerManagement.resumeCommands = "xscreensaver-command -lock"; #Commands executed after the system resumes from suspend-to-RAM.
  services = {
    acpid = {
      enable = true;     # acpi
      lidEventCommands = ''
        # suspend on lid close event or do nothing
        grep -q open /proc/acpi/button/lid/LID0/state && exit 0 || systemctl suspend
      ''; # suspend on lid close
    };
  };
}
