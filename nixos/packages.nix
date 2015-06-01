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
      enableAdobeFlash = true;
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
    bc
    pgadmin
    html2text
    pciutils # lspci, etc...
    nix-prefetch-scripts nix-repl nixops nox
    psmisc # fuser
    python27Packages.screenkey
    texLiveFull
    mysql mysqlWorkbench
    pidgin-with-plugins
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
    gnome3_12.nautilus gnome3_12.gnome_settings_daemon
    gnome3_12.eog pinta scrot
    vlc x264 mplayer
    gnome3_12.zenity
    transmission_gtk
    audacious
    linuxPackages.virtualbox packer vagrant docker
    evince fbreader mcomix
    filezilla
    git gitAndTools.tig gitAndTools.hub gitg meld
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
    python python3 python34Packages.pip
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
       self.xmonad
       self.xmonad-contrib
       self.xmonad-extras
       self.xmobar
       self.pandoc
       # self.ncurses
       self.cabal-install
       # haskell-pack deps
       self.stylish-haskell
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
    cups samba
    telnet
    # rhino
    # nodejs nodePackages.npm nodePackages.jshint nodePackages.grunt-cli nodePackages.npm2nix nodePackages.bower2nix
    ncurses
    # x11vnc tightvnc
    opam
  ];
}
