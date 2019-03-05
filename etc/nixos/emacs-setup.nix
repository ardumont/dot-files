{config, pkgs, ...}:

let emacs = import ./emacs.nix { inherit pkgs; };
  startEmacsServer = pkgs.writeScript "start-emacs-server" ''
    #!/bin/sh
    . ${config.system.build.setEnvironment}
    ${emacs}/bin/emacs --daemon
  '';

  in {
    # Create a systemd user service for emacs daemon. This is useful because
    # systemd will take care of launching emacs in the background and I
    # will just have to connect to it through emacs-client. This is a
    # user service. This means I have to pass the "--user" option to
    # systemd when I want to control the service.

    environment.systemPackages = with pkgs; [
      emacs texinfo w3m
      # emacs dependencies
      notmuch                                  # From main packages set
      offlineimap
      # haskellPackages.structured-haskell-mode  # From haskell set
      # haskellPackages.hlint
      # haskellPackages.hasktags
      # haskellPackages.stylish-haskell
      # ocaml opam gnum4 # aspcud (not yet present in nixos)
      # ocamlPackages.merlin                     # From ocaml set
      dejavu_fonts
    ];

    systemd.user.services.emacs = {
      description = "Emacs: the extensible, self-documenting text editor";
      enable = true;
      environment.SSH_AUTH_SOCK = "%h/.gnupg/S.gpg-agent.ssh";
      serviceConfig = {
        Type = "forking";
        ExecStart = "${startEmacsServer}";
        ExecStop = "${emacs}/bin/emacsclient --eval (kill-emacs)";
        Restart = "always";
      };
      wantedBy = [ "default.target" ];
    };
}
