{ config, pkgs, ... }:

{
  nix.trustedBinaryCaches = [
    http://hydra.nixos.org
    http://cache.nixos.org
    http://hydra.nixos.org
  ];

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
  };

  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    pingus
    sqlite xorg.libX11
    python3 sshfsFuse
    python34Packages.powerline
    powerline-fonts
    stack
    # xorg_sys_opengl mesa_glu freeglut
    qemu
    cabal2nix
    hexedit
    bc
    html2text
    pciutils # lspci, etc...
    nix-prefetch-scripts nix-repl nixops nox
    psmisc # fuser
    python python27Packages.screenkey
    # pidgin-with-plugins
    # androidsdk_4_4
    wmname
    feh
    # steam steamChrootEnv # sudo init-steam-chrootenv mount-steam-chrootenv load-steam-chrootenv
    xsane
    jq
    which
    peco
    nix-repl
    # gnome3_18.nautilus gnome3_18.gnome_settings_daemon
    # gnome3_18.eog
    pinta scrot
    vlc x264 mplayer mpv
    gnome3.zenity
    # transmission_gtk
    # audacious
    # linuxPackages.virtualbox packer vagrant docker
    evince fbreader mcomix
    # filezilla
    git gitAndTools.tig gitg meld
    # gitAndTools.hub
    gnupg gnupg1compat pinentry
    cryptsetup
    pmount file
    wget curl tree
    gcc gnumake qemu
    trayer
    networkmanagerapplet
    x11 xlibs.xmessage xlibs.xmodmap xdotool x11_ssh_askpass xscreensaver xlibs.xbacklight xlibs.xdpyinfo xlibs.xkill xlibs.xhost
    libxml2
    most
    xclip xsel pass pwgen
    htop # powertop
    tmux bind rxvt_unicode urxvt_perls
    bash zsh
    ruby bundler
    zlib
    firefoxWrapper conkeror #chromium
    graphviz
    coreutils lshw lsof
    nmap netcat wireshark
    libzip p7zip unrar zip unzip pigz gnutar pv
    acpi acpid acpitool
    # clojure leiningen jdk
    gparted testdisk
    binutils
    pmutils
    autojump
    inotifyTools
    unetbootin
    alsaUtils
    darcs
    # (haskellngPackages.ghcWithPackages (self : [
    #    self.cabal-install
    #    self.xmonad
    #    self.xmonad-contrib
    #    self.xmonad-extras
    #    self.xmobar
    #    self.pandoc
    #    self.stack
    #    # self.ncurses
    #    # self.cabal2nix
    #    # self.lens
    #    # self.hdevtools
    #    # self.zlib
    #    # self.mtl
    #    # self.HUnit
    #    # self.QuickCheck
    #    # self.hoogle
    # ]))
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
    ### common-lisp
    sbcl
  ];
}