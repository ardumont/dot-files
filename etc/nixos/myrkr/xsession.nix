{ ... }:

{
  programs.ssh.startAgent = false; # do not start agent

  services.xserver = {
    enable = true;
    exportConfiguration = true; # create link /etc/X11/xorg.conf to real conf

    # keyboard
    layout = "us";
    xkbOptions = "eurosign:e,terminate=ctrl_alt_backspace";

    desktopManager = {
      xterm.enable = true;
      gnome3.enable = true;
      mate.enable = true;
    };

    displayManager = {
      defaultSession = "xterm";
      lightdm.enable = true;
    };
  };
}
