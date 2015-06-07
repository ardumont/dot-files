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
  packageOverrides = self : with pkgs; with sourceAndTags;
    # default dev environment with common needed tools
    let defaultDevEnv = { name, buildInputs ? [], cTags ? [], extraCmds ? ""}:
          pkgs.myEnvFun {
            inherit name;
            shell = "${pkgs.zsh}/bin/zsh";
            buildInputs = buildInputs
              ++ map (x : sourceWithTagsDerivation ( (addCTaggingInfo x ).passthru.sourceWithTags ) ) cTags
              ++ [ gitFull zsh emacs tmux gnumake ];
            extraCmds = ''
              # HOME=${builtins.getEnv "HOME"}
              ${extraCmds}
            '';
          };

        defaultJavaEnv = { name, buildInputs ? [] }:
          defaultDevEnv {
            inherit name;
            buildInputs = buildInputs ++ [ maven ant idea.idea-community jd-gui ];
          };

        composeDevEnv = { name, envs }:
          defaultDevEnv {
            inherit name;
            buildInputs = pkgs.lib.concatMap (env: env.buildInputs) envs;
          };

    in rec {

      # override the default pidgin with plugins (empty by default)
      pidgin-plugins = pidgin-with-plugins.override {
        plugins = [ pidginotr skype4pidgin ];
      };

      # xmonad-with-packages = pkgs.xmonad-with-packages.override {
      #   packages = with pkgs.haskellngPackages.ghcWithPackages; [ xmonad-contrib xmonad-extras ];
      # };

      # install: nix-env -i env-sdl
      # load: load-env-sdl
      sdlEnv = self.myEnvFun {
          name = "sdl";
          buildInputs = [ stdenv SDL SDL_image SDL_ttf SDL_gfx cmake SDL_net pkgconfig ];
      };

      # default haskell environment to provide when dev
      # install: nix-env -iA nixos.pkgs.haskell
      #          nix-env -i env-haskell
      # load: load-env-haskell
      hsEnv = defaultDevEnv {
        name = "haskell";
        buildInputs = with pkgs.haskellPackages; [
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

      # default purescript environment
      # install: nix-env -i env-purescript
      # load: load-env-purescript
      purescriptEnv = defaultDevEnv {
        name = "purescript";
        buildInputs = [
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
        buildInputs = with pkgs.haskellPackages; [
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
        buildInputs = [
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
        buildInputs = [
          androidsdk_4_4
          idea.android-studio
          idea.idea-community
          heimdall
        ];
      };

      javaEnv = defaultDevEnv {
        name = "java";
        buildInputs = [
          maven
          ant
          idea.idea-community
          jd-gui
        ];
      };

      java6Env = defaultJavaEnv {
        name = "java6";
        buildInputs = [ oraclejdk tomcat6 ];
      };

      java7Env = defaultJavaEnv {
        name = "java7";
        buildInputs = [ oraclejdk7 ];
      };

      java8Env = defaultJavaEnv {
        name = "java8";
        buildInputs = [ oraclejdk8 ];
      };

      cljEnv = defaultDevEnv {
        name = "clj";
        buildInputs = [ jdk clojure leiningen ];
      };

      emacslispEnv = defaultDevEnv {
        name = "emacslisp";
        buildInputs = [
          emacs
          emacs24Packages.cask
          python
        ];
      };

      wifiToolsEnv = defaultDevEnv {
        name = "wifiTools";
        buildInputs = [
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
        buildInputs = [ jekyll bundler ];
      };

      # idrisEnv = defaultDevEnv {
      #   name = "idris";
      #   buildInputs = [ haskellPackages_ghc783_profiling.idris_plain ];
      # };

      ml-env = defaultDevEnv {
        name = "ml";
        buildInputs = [ opam ocaml ];
      };

      # emacs = emacs24;

#       emacsWithPackages = pkgs.buildEnv {
#         ignoreCollisions = true;
#         name = "emacs-with-packages";
# #        buildInputs = [ mu ];
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
        buildInputs = with pkgs; [
          awscli
          s3cmd
          python34Packages.python
          python34Packages.pip
        ];
      };

      latex = defaultDevEnv {
        name = "latex";
        buildInputs = with pkgs; [
          # texLive
          texLiveFull
          texLiveBeamer
        ];
      };

      vpn = defaultDevEnv {
        name = "vpn";
        buildInputs = with pkgs; [
          openconnect
          networkmanager_openconnect
          openvpn
          networkmanager_openvpn
        ];
      };

      python-dev-git = defaultDevEnv {
        name = "python-dev-git";
        buildInputs = with pkgs; [
          python34
          python34Packages.pygit2
          python34Packages.sqlalchemy9
          python34Packages.psycopg2
          python34Packages.requests
          python34Packages.nose
        ];
      };

      systemToolsEnv = defaultDevEnv {
        name = "system";
        buildInputs = with pkgs; [
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
