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

    packageOverrides = pkgs: {
      # override the default pidgin with plugins (empty by default)
      pidgin-with-plugins = pkgs.pidgin-with-plugins.override {
        plugins = with pkgs; [ pidginsipe pidginotr ];
      };

      openssh = pkgs.lib.overrideDerivation pkgs.openssh (attrs: {
        name = "openssh-6.6p1";
      });

      gitAndTools = pkgs.gitAndTools // {
        git-remote-hg = pkgs.lib.overrideDerivation pkgs.gitAndTools.git-remote-hg (attrs: {
          src = pkgs.fetchgit {
            url = "https://github.com/fingolfin/git-remote-hg.git";
            sha256 = "0ysgklxms0k7dhg84svzcgsz8lxynrb7ydb2l0mgid0r7dpilfp0";
          };
        });
      };
    };
  };

  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    openssh
    which psmisc
    peco
    nix-repl
    pidgin-with-plugins
    gnome3_12.nautilus gnome3_12.gnome_settings_daemon
    gnome3_12.eog pinta scrot
    vlc x264
    gnome3_12.zenity
    transmission_gtk
    audacious
    linuxPackages.virtualbox packer
    evince fbreader
    filezilla
    git gitAndTools.tig gitAndTools.hub gitg meld
    gitAndTools.git-remote-hg
    mercurialFull
    gnupg gnupg1 pinentry
    pmount file
    wget curl tree
    gcc gnumake
    trayer
    networkmanagerapplet
    x11 xlibs.xmessage xlibs.xmodmap xdotool xscreensaver xlibs.xbacklight xlibs.xdpyinfo xlibs.xkill xlibs.xhost
    libxml2
    mosh
    offlineimap mu
    most
    xclip xsel pass keychain
    htop powertop
    emacs texinfo w3m
    tmux bind rxvt_unicode urxvt_perls
    bash zsh ruby
    python3 python34Packages.pip python27Full
    zlib
    firefoxWrapper chromium
    conkeror
    graphviz
    nmap netcat wireshark
    p7zip unrar unzip
    acpi acpid acpitool
    gparted
    binutils
    pmutils
    autojump
    inotifyTools
    #unetbootin
    alsaUtils
    lsof
    vagrant
    darcs
    haskellPackages.pandoc
#    haskellPlatform
#    haskellPlatform.ghc
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
    cups
    samba smbnetfs
    telnet
    # rhino
    nodejs nodePackages.npm nodePackages.jshint nodePackages.grunt-cli nodePackages.bower nodePackages.npm2nix nodePackages.bower2nix
    ncurses
    dos2unix
    wmname
    x11vnc tightvnc autocutsel remmina
    rubyLibs.nokogiri rubyLibs.jekyll rubyLibs.bundler
  ];
}
