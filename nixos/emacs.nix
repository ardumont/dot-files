{config, pkgs, ...}:

let emacs = pkgs.emacsWithPackages
  (with pkgs.emacs24Packages; with pkgs.emacs24PackagesNg; [
    # aspell
    # aspellDicts.en
    # aspellDicts.fr
    flycheck
    flycheck-pos-tip

    cask
    markdown-mode
    markdown-toc
    org-trello
    org2jekyll
    auto-complete
    ac-haskell-process
    company
    haskell-mode
    # structured-haskell-mode
    ace-jump-mode
    exec-path-from-shell
    # gnus
    god-mode
    magit
    projectile
    switch-window
    smart-mode-line
    undo-tree
    use-package
    dash
    # dash-functional
    s
    deferred
    diminish
    popup
    helm
    helm-swoop
  ]);

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
