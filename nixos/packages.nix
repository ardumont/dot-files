{ config, pkgs, ... }:

{
  nix.trustedBinaryCaches = [
    http://hydra.nixos.org
    http://cache.nixos.org
    http://hydra.nixos.org
    http://hydra.cryp.to
  ];

  nixpkgs.config = {
    allowUnfree = true;

    firefox = {
      # enableAdobeFlash = true;
      enableGoogleTalkPlugin = true;
    };

    chromium = {
      enableGoogleTalkPlugin = true;
      enablePepperFlash = true; # Chromium's non-NSAPI alternative to Adobe Flash
      enablePepperPDF = true;
    };

    packageOverrides = pkgs: {
      # override the default pidgin with plugins (empty by default)
      pidgin-with-plugins = pkgs.pidgin-with-plugins.override {
        plugins = with pkgs; [ pidginotr skype4pidgin ];
      };

      # xmonad-with-packages = pkgs.xmonad-with-packages.override {
      #   packages = with pkgs.haskellngPackages.ghcWithPackages; [ xmonad-contrib xmonad-extras ];
      # };
    };

  };

  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    python3
    xorg_sys_opengl mesa_glu freeglut
    cabal2nix
    hexedit
    bc
    html2text
    pciutils # lspci, etc...
    nix-prefetch-scripts nix-repl nixops nox
    psmisc # fuser
    python python27Packages.screenkey
    pidgin-with-plugins
    androidsdk_4_4
    wmname
    feh
    # steam steamChrootEnv # sudo init-steam-chrootenv mount-steam-chrootenv load-steam-chrootenv
    xsane
    jq
    which
    peco
    nix-repl
    gnome3_16.nautilus gnome3_16.gnome_settings_daemon
    gnome3_16.eog pinta scrot
    vlc x264 mplayer
    gnome3_16.zenity
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
    bittorrentSync20
    trayer
    networkmanagerapplet
    x11 xlibs.xmessage xlibs.xmodmap xdotool x11_ssh_askpass xscreensaver xlibs.xbacklight xlibs.xdpyinfo xlibs.xkill xlibs.xhost
    libxml2
    mosh
    offlineimap notmuch
    most
    xclip xsel pass pwgen keychain
    htop # powertop
    emacs texinfo w3m emacs24Packages.cask aspell aspellDicts.en aspellDicts.fr
    # emacs24PackagesNg.structured-haskell-mode
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
    (haskellngPackages.ghcWithPackages (self : [
       self.cabal-install
       self.xmonad
       self.xmonad-contrib
       self.xmonad-extras
       self.xmobar
       self.pandoc
       self.stack
       # self.ncurses
       # haskell-pack deps
       self.stylish-haskell
       self.structured-haskell-mode
       self.hlint
       self.hasktags
       # self.cabal2nix
       # self.lens
       # self.hdevtools
       # self.zlib
       # self.mtl
       # self.HUnit
       # self.QuickCheck
       # self.hoogle
    ]))
    rlwrap
    fortune cowsay
    ffmpeg
    simplescreenrecorder
    keymon
    imagemagick
    libreoffice
    ### printing
    cups samba
    telnet
    ncurses
    ### javascript stack
    # rhino
    # nodejs nodePackages.npm nodePackages.jshint nodePackages.grunt-cli nodePackages.npm2nix nodePackages.bower2nix
    ### communication
    # x11vnc tightvnc
    ### ocaml
    opam
    ### common-lisp
    sbcl
  ];
}
