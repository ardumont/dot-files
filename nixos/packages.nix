{ config, pkgs, ... }:

{
  nix.trustedBinaryCaches = [ "http://hydra.nixos.org" "http://cache.nixos.org" ];

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
  };

  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    nix-repl
    gnome3.nautilus gnome3.gnome_settings_daemon
    gnome3.eog pinta scrot
    gnome3.totem vlc mplayer2 x264
    transmission_gtk
    audacious
    linuxPackages.virtualbox
    evince fbreader
    filezilla
    gnome.zenity
    git gitAndTools.tig gitAndTools.hub gitg meld
    mercurialFull
    gnupg gnupg1 pinentry
    pmount file
    wget curl tree
    gcc gnumake
    dropbox dropbox-cli
    trayer
    networkmanagerapplet
    x11 xlibs.xmessage xlibs.xmodmap xdotool x11_ssh_askpass xscreensaver xlibs.xbacklight xlibs.xdpyinfo xlibs.xkill
    mosh
    offlineimap mu
    most
    xclip pass keychain
    htop powertop
    emacs texinfo w3m
    tmux rxvt_unicode bind
    tcsh bash zsh python ruby
    zlib
    firefoxWrapper chromium conkeror
    graphviz
    nmap netcat wireshark
    p7zip unrar unzip
    acpi acpid acpitool
    mplayer vlc
    clojure leiningen jdk
    gparted
    binutils
    pmutils
    autojump
    inotifyTools
    unetbootin
    alsaUtils
    lsof
    vagrant
    darcs
#    rubyLibs.bundler
    haskellPackages.pandoc
    haskellPlatform
    haskellPlatform.ghc
    haskellPackages.ncurses
    (haskellPackages.ghcWithPackages (self : [
       self.cabalInstall
       self.xmonad
       self.xmonadContrib
       self.xmonadExtras
       self.xmobar
       self.cabal2nix
       self.lens
       self.hlint
       self.hdevtools
       self.zlib
       self.mtl
       self.HUnit
       self.QuickCheck
       self.hoogle
       # self.Agda
    ]))
    rlwrap
    fortune cowsay
    ffmpeg
    simplescreenrecorder
    keymon
    imagemagick
    libreoffice
    docker
    cups samba
    telnet
    transmission_gtk
    nodejs rhino nodePackages.npm nodePackages.jshint nodePackages.grunt-cli nodePackages.npm2nix nodePackages.bower2nix
    ncurses
  ];
}
