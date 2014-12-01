# configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running nixos-help).

{ config, pkgs, ... }:

{

  imports = [ ./proxy.nix ];

  networking = {
#    firewall.enable = false;
#    firewall.allowedTCPPorts = [ 80 443 3000 22 ];

    hostName = "nixy-vagrant";    # Define your hostname.

    # wireless = {
    #   enable = true;                # Enables wireless
    #   userControlled.enable = true; # user can play with wifi
    #   interfaces = [ "wlp1s0" ];    # explicit the interfaces the user can modify
    # };

    networkmanager.enable = true; # nm, nmcli, etc... incompatible with networking.wireless
    extraHosts = ''
    '';
  };

  time.timeZone = "Europe/Paris";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

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
    python python34 pypy python34Packages.pip
    peco
    nix-repl
    gnome3_12.nautilus gnome3_12.gnome_settings_daemon gnome3_12.zenity
    gnome3_12.evolution gnome3_12.evolution_data_server
    gnome3_12.eog pinta scrot
#    gnome3_12.totem vlc mplayer2 x264
    transmission_gtk
    audacious
#    linuxPackages.virtualbox
    evince fbreader
    filezilla
    git gitAndTools.tig gitAndTools.hub gitg mercurialFull gitAndTools.git-remote-hg
    gnupg gnupg1 pinentry
    pmount file
    wget curl tree
    gcc gnumake
#    dropbox dropbox-cli
    trayer
    networkmanagerapplet
    x11 xlibs.xmessage xlibs.xmodmap xdotool x11_ssh_askpass xscreensaver xlibs.xbacklight xlibs.xdpyinfo xlibs.xkill
    mosh
    offlineimap mu
    most
    xsel xclip pass keychain
    htop powertop
    emacs texinfo w3m
    tmux rxvt_unicode bind
#    tcsh
    bash zsh ruby
    zlib
    firefoxWrapper chromium conkeror
    graphviz
    nmap netcat wireshark
    p7zip unrar unzip
    acpi acpid acpitool
    mplayer vlc
    clojure leiningen jdk maven
    gparted
    binutils
    pmutils
    autojump
    inotifyTools
#    unetbootin
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
#       # self.Agda
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
#    rhino
    ncurses
  ];

  programs.ssh.startAgent = false; # do not start agent (gpg-agent will be started)

  powerManagement.resumeCommands = "xscreensaver-command -lock"; #Commands executed after the system resumes from suspend-to-RAM.

  # List services that you want to enable:
  services = {
    # Whether to enable the WINS NSS (Name Service Switch) plug-in.
    # Enabling it allows applications to resolve WINS/NetBIOS names (a.k.a. Windows machine names) by transparently querying the winbindd daemon.
   samba.nsswins = true;

    acpid = {
      enable = true;     # acpi
      lidEventCommands = ''
        # suspend on lid close event or do nothing
        grep -q open /proc/acpi/button/lid/LID0/state && exit 0 || systemctl suspend
        ''; # suspend on lid close
      };

    locate = {
      enable = true;
      period = "00 19 * * *"; # update db at 19h every day
    };

    openssh.enable = true;
    ntp.enable = true;

    xserver = {
      enable = true;

      startGnuPGAgent = true;
      desktopManager.default = "none";
      layout = "us";
      xkbOptions = "eurosign:e,ctrl:nocaps,terminate=ctrl_alt_backspace,altwin:meta_alt";

      # touchpad
      synaptics.enable = true;
      synaptics.twoFingerScroll = true;

      windowManager = {
        default = "xmonad";
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = haskellPackages: [
            haskellPackages.xmonad
            haskellPackages.xmonadContrib
            haskellPackages.xmobar
          ];
        };
      };
      vaapiDrivers = [ pkgs.vaapiIntel ];
    };

    # https://nixos.org/wiki/Printers
    printing = {
      enable = true;
      drivers = [ pkgs.gutenprint pkgs.hplip ];
    };

    nixosManual.showManual = true; # Add the NixOS Manual on virtual console 8
  };

  fonts = {
    # enableGhostscriptFonts = true;
    # enableCoreFonts = true; # M$'s proprietary Core Fonts.
    enableFontConfig = true;
    enableFontDir = true;
    fonts = [
       pkgs.dejavu_fonts
       # pkgs.andagii
       # pkgs.anonymousPro
       # pkgs.arkpandora_ttf
       # pkgs.bakoma_ttf
       # pkgs.cantarell_fonts
       # pkgs.corefonts
       # pkgs.clearlyU
       # pkgs.cm_unicode
       # pkgs.freefont_ttf
       # pkgs.gentium
       # pkgs.inconsolata
       # pkgs.liberation_ttf
       # pkgs.libertine
       # pkgs.lmodern
       # pkgs.mph_2b_damase
       # pkgs.oldstandard
       # pkgs.theano
       # pkgs.tempora_lgc
       # pkgs.terminus_font
       # pkgs.ttf_bitstream_vera
       # pkgs.ttf_bitstream_vera_for_powerline
       # pkgs.ucsFonts
       # pkgs.unifont
       # pkgs.vistafonts
       # pkgs.wqy_zenhei
    ];
  };

  nix.trustedBinaryCaches = [ "http://hydra.nixos.org" "http://cache.nixos.org" ];

  users = {
    defaultUserShell = "/var/run/current-system/sw/bin/zsh";

    # Define a user account. Don't forget to set a password with passwd.
    extraUsers = [{
      description = "Antoine R. Dumont";
      name = "tony";
      group = "users";
      uid = 1000;
      createHome = true;
      home = "/home/tony";
      #password = "pass-here";
      extraGroups = [ "users" "wheel" "audio" "video" "vboxusers" "docker" ];
      useDefaultShell = true;
    }];
  };

  security = {
    sudo.configFile = ''
      root   ALL=(ALL) SETENV: ALL
      %wheel ALL=(ALL) SETENV: ALL
      Defaults env_keep = "http_proxy https_proxy ftp_proxy no_proxy"
    '';
    setuidPrograms = [ "pmount" "pumount" ];
  };
}