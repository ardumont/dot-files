{ pkgs }:

{
  firefox = {
    enableAdobeFlash = true;
    enableGoogleTalkPlugin = true;
  };

  chromium = {
    enableGoogleTalkPlugin = true;
    enablePepperFlash = true; # Chromium's non-NSAPI alternative to Adobe Flash
    enablePepperPDF = true;
  };

  # Allow broken content (when using nixpkgs as git)
  allowBroken = true;

  # Allow unfree license
  allowUnfree = true;

  # source: https://nixos.org/wiki/Howto_develop_software_on_nixos
  packageOverrides = self : with pkgs;
    rec {
      # default dev environment with common needed tools
      # default haskell environment to provide when dev
      haskellToolsEnv = buildEnv {
        name = "haskellTools";
        paths = with pkgs.haskellngPackages; [
          cabal-install
          cabal2nix
          lens
          hlint
          hdevtools
          zlib
          mtl
          HUnit
          QuickCheck
          hoogle
          # Agda
        ];
        # paths = [
        #   (haskellngPackages.ghcWithPackages (self : [
        #      self.xmonad
        #      self.xmonad-contrib
        #      self.xmonad-extras
        #      self.xmobar
        #      self.pandoc
        #      self.ncurses
        #      self.cabal-install
        #      # haskell-pack deps
        #      self.stylish-haskell
        #      self.hasktags
        #      self.cabal2nix
        #      self.lens
        #      self.hdevtools
        #      self.zlib
        #      self.mtl
        #      self.HUnit
        #      self.QuickCheck
        #      self.hoogle
        #   ]))
        # ];
      };

     openGLToolsEnv = buildEnv {
       name = "openGLTools";
       paths = [
         xorg_sys_opengl
         mesa_glu
         freeglut
       ];
     #   CABAL_INSTALL_EXTRA_FLAGS = ''
     #       --extra-lib-dirs=$xorg_sys_opengl/lib \
     #       --extra-lib-dirs=$mesa_glu/lib \
     #       --extra-lib-dirs=$freeglut/lib \
     #       --extra-include-dirs=$xorg_sys_opengl/include \
     #       --extra-include-dirs=$mesa_glu/include \
     #       --extra-include-dirs=$freeglut/include \
     #   '';
     };

      # override the default pidgin with plugins (empty by default)
      pidgin-plugins = pidgin-with-plugins.override {
        plugins = [ pidginotr skype4pidgin ];
      };

      # xmonad-with-packages = pkgs.xmonad-with-packages.override {
      #   packages = with pkgs.haskellngPackages.ghcWithPackages; [ xmonad-contrib xmonad-extras ];
      # };

      # default purescript environment
      # install: nix-env -i env-purescript
      # load: load-env-purescript
      purescriptToolsEnv = buildEnv {
        name = "purescriptTools";
        paths = [
          nodejs
          haskellPackages.purescript
          nodePackages.npm
          nodePackages.npm2nix
          nodePackages.jshint
          nodePackages.grunt-cli
          nodePackages.bower
        ];
      };

      # default xmonad env
      # install: nix-env -i env-xmonad
      # load: load-env-xmonad
      xmonadToolsEnv = buildEnv {
        name = "xmonadTools";
        paths = with pkgs.haskellPackages; [
         xmonad
         xmonad-contrib
         xmonad-extras
         xmobar
        ];
      };

      # default mynodejs environment
      # install: nix-env -i env-mynodejs
      # load: load-env-mynodejs
      nodeJSToolsEnv = buildEnv {
        name = "nodeJSTools";
        paths = [
          nodejs
          nodePackages.npm
          nodePackages.jshint
          nodePackages.bower
          nodePackages.grunt-cli
          nodePackages.npm2nix
          nodePackages.bower2nix
          nodePackages.phantomjs
          nodePackages.nodemon
          chromedriver
          # rhino
          # nodejs nodePackages.npm nodePackages.jshint nodePackages.grunt-cli nodePackages.npm2nix nodePackages.bower2nix
        ];
      };

      androidToolsEnv = buildEnv {
        name = "androidTools";
        paths = [
          androidsdk_4_4
          idea.android-studio
          idea.idea-community
          heimdall
        ];
      };

      javaToolsEnv = buildEnv {
        name = "javaTools";
        paths = [
          maven
          ant
          idea.idea-community
          jd-gui
        ];
      };

      java6ToolsEnv = buildEnv {
        name = "java6Tools";
        paths = [ oraclejdk tomcat6 ];
      };

      java7ToolsEnv = buildEnv {
        name = "java7Tools";
        paths = [ oraclejdk7 ];
      };

      java8ToolsEnv = buildEnv {
        name = "java8Tools";
        paths = [ oraclejdk8 ];
      };

      clojureToolsEnv = buildEnv {
        name = "clojureTools";
        paths = [ jdk clojure leiningen ];
      };

      # trying to enhance default environment setup for emacs (not tested yet)
      emacs24 = self.emacs24.overrideDerivation (args: rec {
        # withGTK3 = true;
        # withGTK2 = false;
        buildInputs = (args.buildInputs ++ [
          makeWrapper
          python3
          python34Packages.elpy
          python27Packages.rope
          python34Packages.virtualenv
          python34Packages.jedi
          python34Packages.flake8
          python34Packages.pyflakes
          python34Packages.autopep8
          python34Packages.pep8
          python34Packages.importmagic
        ]);
        postInstall = with python34Packages; (args.postInstall + ''
          echo "This is PYTHONPATH: " $PYTHONPATH
          wrapProgram $out/bin/emacs \
              --prefix PYTHONPATH : "$(toPythonPath ${python3}):$(toPythonPath ${ipython}):$(toPythonPath ${pip}):$PYTHONPATH" \
              --prefix PYTHONPATH : "$(toPythonPath ${virtualenv})" \
              --prefix PYTHONPATH : "$(toPythonPath ${elpy})" \
              --prefix PYTHONPATH : "$(toPythonPath ${jedi})" \
              --prefix PYTHONPATH : "$(toPythonPath ${autopep8})" \
              --prefix PYTHONPATH : "$(toPythonPath ${pep8})" \
              --prefix PYTHONPATH : "$(toPythonPath ${flake8})" \
              --prefix PYTHONPATH : "$(toPythonPath ${pyflakes})" \
              --prefix PYTHONPATH : "$(toPythonPath ${importmagic})" \
              --prefix PYTHONPATH : "$(toPythonPath ${python27Packages.rope})";
         '');
      });

      # distribution emacs with other packages
      emacsToolsEnv = pkgs.buildEnv {
        name = "emacsTools";
#        ignoreCollisions = true;
        paths = [
          (emacsWithPackages
            (with emacs24PackagesNg; [
              aspell
              aspellDicts.en
              aspellDicts.fr
              flycheck
              flycheck-pos-tip

              markdown-mode
              markdown-toc
              org-trello
              org2jekyll
              auto-complete
              ac-haskell-process
              company
              haskell-mode
              structured-haskell-mode
              ace-jump-mode
              exec-path-from-shell
#             gnus
              god-mode
              magit
              projectile
              switch-window
              smart-mode-line
              undo-tree
              use-package
              dash
              dash-functional
              s
              deferred
              diminish
              popup
              helm
              helm-swoop
              elpy
              # ag
              # auctex
              # change-inner
              # circe
              # expand-region
              # hi2
              # idris-mode
              # monokai-theme
              # org-plus-contrib
              # smartparens
              # volatile-highlights
              # wgrep
              # zenburn-theme

              emacs24Packages.cask
            ])
          )];
        };

      commonLispToolsEnv = buildEnv {
        name = "commonLispTools";
        paths = [
          sbcl
        ];
      };

      wifiToolsEnv = buildEnv {
        name = "wifiTools";
        paths = [
          bridge_utils
          wirelesstools
          hostapd
        ];
      };

      staticSiteToolsEnv = buildEnv {
        name = "staticSiteTools";
        ignoreCollisions = true;
        paths = [ jekyll bundler ruby_2_1_1 libffi ];
      };

      idrisToolsEnv = buildEnv {
        name = "idrisTools";
        paths = [ haskellPackages_ghc783_profiling.idris_plain ];
      };

      mlToolsEnv = buildEnv {
        name = "mlTools";
        paths = [ opam ocaml gnum4 ncurses ];
      };

      awsToolsEnv = buildEnv {
        name = "awsTools";
        paths = with pkgs; [
          awscli
          s3cmd
          python34Packages.python
          python34Packages.pip
        ];
      };

      latexToolsEnv = buildEnv {
        name = "latexTools";
        paths = with pkgs; [
          # texLive
          texLiveFull
          texLiveBeamer
        ];
      };

      vpnToolsEnv = buildEnv {
        name = "vpnTools";
        paths = with pkgs; [
          openconnect
          networkmanager_openconnect
          openvpn
          networkmanager_openvpn
        ];
      };

      python3DevToolsEnv = buildEnv {
        name = "python3DevTools";
        paths = with pkgs; with python34Packages; [
          python34
          ipython
          pip

          python27Packages.rope

          elpy
          virtualenv
          jedi
          flake8
          pyflakes
          autopep8
          pep8
          #importmagic

          pygit2
          sqlalchemy9
          psycopg2
          requests
          nose
          flask
        ];
      };

      tchatToolsEnv = buildEnv {
        name = "tchatTools";
        paths = with pkgs; [
          pidgin-plugins
          skype
        ];
      };

      podcastToolsEnv = buildEnv {
        name = "podcastTools";
        paths = with pkgs; [
          python27Packages.screenkey
          keymon
          simplescreenrecorder
        ];
      };

      mysqlToolsEnv = buildEnv {
        name = "mysqlTools";
        paths = with pkgs; [
          mysql
          mysqlWorkbench
        ];
      };

      pgsqlToolsEnv = buildEnv {
        name = "pgsqlTools";
        paths = [
          pgadmin
          postgresql
        ];
      };

      nixToolsEnv = buildEnv {
        name = "nixTools";
        paths = [
          nix-prefetch-scripts
          nix-repl
          nixops
          nox
        ];
      };

      mailToolsEnv = buildEnv {
        name = "mailTools";
        paths = [
           offlineimap
           notmuch
           # mu
        ];
      };

      phabricatorToolsEnv = buildEnv {
        name = "phabricatorTools";
        paths = [
           # phabricator
           arcanist
        ];
      };

      shellToolsEnv = buildEnv {
        name = "shellTools";
        paths = [
          bc
          pciutils # lspci, etc...
          psmisc # fuser
          which
          hexedit
          pmount file
          tree
          gcc gnumake
          #most
          pass
          xclip xsel pwgen keychain
          html2text
          wmname
          feh
          # texLiveFull
          jq
          peco
          htop # powertop
          filezilla
          gnupg pinentry
          autojump
          inotifyTools
          alsaUtils
          lsof
          rlwrap
          fortune cowsay
          tmux bind rxvt_unicode urxvt_perls
          bash zsh
          graphviz
          p7zip unrar zip unzip
          acpi acpid acpitool
          gparted testdisk
          binutils
          pmutils
          unetbootin
          telnet
          ncurses
          ncdu
          urlview
        ];
      };

      syncToolsEnv = buildEnv {
        name = "syncTools";
        paths = [ syncthing ];
      };

      dvcsToolsEnv = buildEnv {
        name = "dvcsTools";
        paths = [
          git
          gitAndTools.tig
          gitAndTools.hub
          gitg
          meld
          libgit2
          darcs
        ];
      };

      readerToolsEnv = buildEnv {
        name = "readerTools";
        paths = [
          #evince
          fbreader
          mcomix
          apvlv
        ];
      };

      multimediaToolsEnv = buildEnv {
        name = "multimediaTools";
        paths = [
          gnome3.eog
          pinta
          scrot
          vlc
          x264
          # mplayer
          audacious
          ffmpeg
          imagemagick
          # gimp
        ];
      };

      gnome3ToolsEnv = buildEnv {
        name = "gnome3Tools";
        paths = [
          gnome3.nautilus gnome3.gnome_settings_daemon
          gnome3.zenity
          transmission_gtk
        ];
      };

      virtualToolsEnv = buildEnv {
        name = "virtualTools";
        paths = [
          linuxPackages.virtualbox
          packer
          vagrant
          docker
        ];
      };

      networkToolsEnv = buildEnv {
        name = "networkTools";
        paths = [
          networkmanagerapplet
          nmap
          netcat
          wireshark
          # x11vnc tightvnc
          ansible
          # arcanist
          # aria
          cacert
          # fping
          # httrack
          iperf
          mosh
          mtr
          openssl
          rsync
          # socat2pre
          # spiped
          wget
          youtubeDL
          # znc
          openssh
          curl
        ];
      };

      steamToolsEnv = buildEnv {
        name = "steamTools";
        paths = [
          steam
          steamChrootEnv # steam steamChrootEnv # sudo init-steam-chrootenv mount-steam-chrootenv load-steam-chrootenv
        ];
      };

      printScanToolsEnv = buildEnv {
        name = "printScanTools";
        paths = [
          xsane
          cups
          samba
        ];
      };

      wysiwygEditorToolsEnv = buildEnv {
        name = "wysiwygEditorTools";
        paths = [
          libreoffice
        ];
      };

      browserToolsEnv = buildEnv {
        name = "browserTools";
        paths = [
          firefoxWrapper
          conkeror
          #chromium
        ];
      };

      libDevToolsEnv = buildEnv {
        name = "libDevTools";
        paths = [
          libxml2
          zlib
        ];
      };

      x11ToolsEnv = buildEnv {
        name = "x11Tools";
        paths = with pkgs; [
          dejavu_fonts
          trayer
          x11
          xlibs.xmessage
          xlibs.xmodmap
          xdotool
          x11_ssh_askpass
          xlibs.xbacklight
          xlibs.xdpyinfo
          xlibs.xkill
          xlibs.xhost
          xscreensaver
        ];
      };

   };
}
