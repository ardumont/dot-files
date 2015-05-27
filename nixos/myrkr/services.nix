{config, pkgs, ...}:

{
  # List services that you want to enable:
  services = {

    locate = {
      enable = true;
      period = "00 19 * * *"; # update db at 19h every day
    };

    openssh.enable = true;
    ntp.enable = true;

    nixosManual.showManual = true; # Add the NixOS Manual on virtual console 8
  };

  # Create a systemd user service for emacs daemon. This is useful because
  # systemd will take care of launching emacs in the background and I
  # will just have to connect to it through emacs-client. This is a
  # user service. This means I have to pass the "--user" option to
  # systemd when I want to control the service.
  systemd.user.services = {
    emacs = {
      description = "Emacs: the extensible, self-documenting text editor";

      serviceConfig = {
        Type      = "forking";
        ExecStart = "${pkgs.emacs}/bin/emacs --daemon";
        ExecStop  = "${pkgs.emacs}/bin/emacsclient --eval \"(kill-emacs)\"";
        Restart   = "always";
      };

      # I want the emacs service to be started with the rest of the user services
      wantedBy = [ "default.target" ];

      # Annoyingly, systemd doesn't pass any environment variable to its
      # services. Below, I set some variables that I missed.
      environment = {
        # Some variables for GTK applications I will launch from Emacs
        # (typically evince and the gnome-terminal)
        GTK_DATA_PREFIX = config.system.path;
        GTK_PATH        = "${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0";

        # Make sure aspell will find its dictionaries
        ASPELL_CONF     = "dict-dir /run/current-system/sw/lib/aspell";

        # Make sure locate will find its database
        LOCATE_PATH     = "/var/cache/locatedb";
      };

      # requisite = [ "display-manager.service" "network-manager.service" ];

      enable = true;
    };
  };

}
