{ pkgs,
  emacsPackagesNg ? (import <nixpkgs> {}).emacsPackagesNg }:

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
    # default dev environment with common needed tools
    let defaultDevEnv = { name, paths ? []}:
          pkgs.buildEnv {
            inherit name;
            paths = paths
              ++ [ gitFull zsh emacs tmux gnumake rlwrap ];
          };

        defaultJavaEnv = { name, paths ? [] }:
          defaultDevEnv {
            inherit name;
            paths = paths ++ [ maven ant idea.idea-community jd-gui ];
          };

        composeDevEnv = { name, envs }:
          defaultDevEnv {
            inherit name;
            paths = pkgs.lib.concatMap (env: env.paths) envs;
          };

    in rec {


      # default haskell environment to provide when dev
      # install: nix-env -iA nixos.pkgs.haskell
      #          nix-env -i env-haskell
      # load: load-env-haskell
      hsEnv = buildEnv {
        name = "haskell";
        paths = with pkgs.haskellPackages; [
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
      };

     # openGLEnv = defaultDevEnv {
     #   name = "opengl";
     #   buildInputs = [ xorg_sys_opengl mesa_glu freeglut ];
     #   CABAL_INSTALL_EXTRA_FLAGS = ''
     #       --extra-lib-dirs=$xorg_sys_opengl/lib \
     #       --extra-lib-dirs=$mesa_glu/lib \
     #       --extra-lib-dirs=$freeglut/lib \
     #       --extra-include-dirs=$xorg_sys_opengl/include \
     #       --extra-include-dirs=$mesa_glu/include \
     #       --extra-include-dirs=$freeglut/include \
     #   '';
     # };

     # snakeDevEnv = composeDevEnv {
     #    name = "snake";
     #    envs = [ hsEnv openGLEnv ];
     #  };

      # override the default pidgin with plugins (empty by default)
      pidgin-plugins = pidgin-with-plugins.override {
        plugins = [ pidginotr skype4pidgin ];
      };

      # xmonad-with-packages = pkgs.xmonad-with-packages.override {
      #   packages = with pkgs.haskellngPackages.ghcWithPackages; [ xmonad-contrib xmonad-extras ];
      # };

      # install: nix-env -i env-sdl
      # load: load-env-sdl
      sdlEnv = buildEnv {
          name = "sdl";
          paths = [ stdenv SDL SDL_image SDL_ttf SDL_gfx cmake SDL_net pkgconfig ];
      };

      # default purescript environment
      # install: nix-env -i env-purescript
      # load: load-env-purescript
      purescriptEnv = defaultDevEnv {
        name = "purescript";
        paths = [
          haskellPackages.purescript
          nodejs
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
      xmonadEnv = defaultDevEnv {
        name = "xmonad";
        paths = with pkgs.haskellPackages; [
         xmonad
         xmonadContrib
         xmonadExtras
         xmobar
        ];
      };

      # default mynodejs environment
      # install: nix-env -i env-mynodejs
      # load: load-env-mynodejs
      myNodeJSEnv = defaultDevEnv {
        name = "mynodejs";
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
        ];
      };

      myAndroidEnv = defaultDevEnv {
        name = "myandroid";
        paths = [
          androidsdk_4_4
          idea.android-studio
          idea.idea-community
          heimdall
        ];
      };

      javaEnv = defaultDevEnv {
        name = "java";
        paths = [
          maven
          ant
          idea.idea-community
          jd-gui
        ];
      };

      java6Env = defaultJavaEnv {
        name = "java6";
        paths = [ oraclejdk tomcat6 ];
      };

      java7Env = defaultJavaEnv {
        name = "java7";
        paths = [ oraclejdk7 ];
      };

      java8Env = defaultJavaEnv {
        name = "java8";
        paths = [ oraclejdk8 ];
      };

      cljEnv = defaultDevEnv {
        name = "clj";
        paths = [ jdk clojure leiningen ];
      };

      emacslispEnv = defaultDevEnv {
        name = "emacslisp";
        paths = [
          emacs
          emacs24Packages.cask
          python
        ];
      };

      wifiToolsEnv = defaultDevEnv {
        name = "wifiTools";
        paths = [
          bridge_utils
          wirelesstools
          hostapd
        ];
      };

      proEnv = composeDevEnv {
        name = "pro";
        envs = [ myAndroidEnv java6Env myNodeJSEnv ];
      };

      jekyllEnv = defaultDevEnv {
        name = "static-site";
        paths = [ jekyll bundler ruby_2_1_1 ];
      };

      # idrisEnv = defaultDevEnv {
      #   name = "idris";
      #   paths = [ haskellPackages_ghc783_profiling.idris_plain ];
      # };

      mlEnv = defaultDevEnv {
        name = "ml";
        paths = [ opam ocaml gnum4 ncurses ];
      };

      # emacs = emacs24;

#       emacsWithPackages = pkgs.buildEnv {
#         ignoreCollisions = true;
#         name = "emacs-with-packages";
# #        paths = [ mu ];
#         paths = with emacsPackagesNg; [
#           emacs24
#           markdown-mode
#           org-trello
#           auto-complete
#           ac-haskell-process
#           company
#           haskell-mode
#           structured-haskell-mode
#           ace-jump-mode
#           exec-path-from-shell
#           flycheck
#           flycheck-pos-tip
#           gnus
#           god-mode
#           aspell
#           aspellDicts.en
#           magit
#           projectile
#           switch-window
#           smart-mode-line
#           undo-tree
#           use-package
#           dash
#           s
#           deferred
#           diminish
#           popup

#           # ag
#           # auctex
#           # change-inner
#           # circe
#           # expand-region
#           # helm
#           # helm-swoop
#           # hi2
#           # idris-mode
#           # monokai-theme
#           # org-plus-contrib
#           # smartparens
#           # volatile-highlights
#           # wgrep
#           # zenburn-theme
#         ];
#       };

      aws = defaultDevEnv {
        name = "aws";
        paths = with pkgs; [
          awscli
          s3cmd
          python34Packages.python
          python34Packages.pip
        ];
      };

      latex = defaultDevEnv {
        name = "latex";
        paths = with pkgs; [
          # texLive
          texLiveFull
          texLiveBeamer
        ];
      };

      vpn = defaultDevEnv {
        name = "vpn";
        paths = with pkgs; [
          openconnect
          networkmanager_openconnect
          openvpn
          networkmanager_openvpn
        ];
      };

      python-dev-git = defaultDevEnv {
        name = "python-dev-git";
        paths = with pkgs; [
          python34
          python34Packages.pygit2
          python34Packages.sqlalchemy9
          python34Packages.psycopg2
          python34Packages.requests
          python34Packages.nose
          python34Packages.ipython
        ];
      };

      systemToolsEnv = defaultDevEnv {
        name = "system";
        paths = with pkgs; [
          hexedit
          bc
          pgadmin
          html2text
          pciutils # lspci, etc...
          nix-prefetch-scripts nix-repl nixops nox
          psmisc # fuser
          python python27Packages.screenkey
          python3 python34Packages.pip python34Packages.flake8 python34Packages.pygit2
          # texLiveFull
          mysql mysqlWorkbench
          pidgin-plugins
          skype
          openvpn networkmanager_openvpn
          androidsdk_4_4
          wmname
          feh
          # steam steamChrootEnv # sudo init-steam-chrootenv mount-steam-chrootenv load-steam-chrootenv
          xsane
          jq
          which
          peco
          nix-repl
          gnome3.nautilus gnome3.gnome_settings_daemon
          gnome3.eog pinta scrot
          vlc x264 mplayer
          gnome3.zenity
          transmission_gtk
          audacious
          linuxPackages.virtualbox packer vagrant docker
          evince fbreader mcomix
          filezilla
          git gitAndTools.tig gitAndTools.hub gitg meld libgit2
          gnupg gnupg1 pinentry
          pmount file
          wget curl tree
          gcc gnumake
          dropbox dropbox-cli
          trayer
          networkmanagerapplet
          x11 xlibs.xmessage xlibs.xmodmap xdotool x11_ssh_askpass xscreensaver xlibs.xbacklight xlibs.xdpyinfo xlibs.xkill xlibs.xhost
          libxml2
          mosh
          offlineimap mu
          most
          xclip xsel pass pwgen keychain
          htop # powertop
          emacs texinfo w3m emacs24Packages.cask aspell aspellDicts.en aspellDicts.fr
          emacs24PackagesNg.structured-haskell-mode
          tmux bind rxvt_unicode urxvt_perls
          bash zsh ruby
          bundler
          zlib
          firefoxWrapper conkeror chromium
          graphviz
          nmap netcat wireshark
          p7zip unrar zip unzip
          acpi acpid acpitool
          clojure leiningen jdk
          gparted testdisk
          binutils
          pmutils
          autojump
          inotifyTools
          unetbootin
          alsaUtils
          lsof
          darcs
          # (haskellngPackages.ghcWithPackages (self : [
             # self.xmonad
             # self.xmonad-contrib
             # self.xmonad-extras
             # self.xmobar
             # self.pandoc
             # self.ncurses
             # self.cabal-install
             # haskell-pack deps
             # self.stylish-haskell
             # self.hasktags
             # self.cabal2nix
             # self.lens
             # self.hdevtools
             # self.zlib
             # self.mtl
             # self.HUnit
             # self.QuickCheck
             # self.hoogle
          # ]))
          rlwrap
          fortune cowsay
          ffmpeg
          simplescreenrecorder
          keymon
          imagemagick
          libreoffice
          cups samba
          telnet
          # rhino
          # nodejs nodePackages.npm nodePackages.jshint nodePackages.grunt-cli nodePackages.npm2nix nodePackages.bower2nix
          ncurses
          # x11vnc tightvnc
          opam
        ];
      };

   };
}
