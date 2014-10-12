# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
      ./hardware-configuration.nix # Include the results of the hardware scan.
    ];

  boot = {
    loader.grub = {
      enable = true;           # Use GRUB boot loader.
      version = 2;             # Version 2
      device = "/dev/sda";     # which hard drive to install it
      memtest86.enable = true; # Activate the check on memory
    };

    extraModprobeConfig = ''
      options snd slots=snd-hda-intel
      options snd_hda_intel enable=0,1
    '';

    kernel.sysctl."fs.inotify.max_user_watches" = 100000;
  };

  networking = {
    hostName = "dagobah";    # Define your hostname.

    # wireless = {
    #   enable = true;                # Enables wireless
    #   userControlled.enable = true; # user can play with wifi
    #   interfaces = [ "wlp1s0" ];    # explicit the interfaces the user can modify
    # };

    networkmanager.enable = true; # nm, nmcli, etc... incompatible with networking.wireless
    extraHosts = ''
      192.168.0.10 dagobah
      192.168.0.11 chris-host
      192.168.0.13 job
      192.168.0.14 myrkr
      192.168.0.20 nas
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
    git gitAndTools.tig gitAndTools.hub gitg
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
    tcsh bash zsh python ruby2
    zlib
    firefoxWrapper chromiumWrapper conkeror
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
    haskellPackages.haskellPlatform
    haskellPackages.haskellPlatform.ghc
    (haskellPackages.ghcWithPackages (self : [
       self.cabalInstall_1_18_0_3
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
  ];

  programs.ssh.startAgent = false; # do not start agent (gpg-agent will be started)

  powerManagement.resumeCommands = "xscreensaver-command -lock"; #Commands executed after the system resumes from suspend-to-RAM.

  # List services that you want to enable:
  services = {
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

    # Define a user account. Don't forget to set a password with ‘passwd’.
    extraUsers = [{
      description = "Antoine R. Dumont";
      name = "tony";
      group = "users";
      uid = 1000;
      createHome = true;
      home = "/home/tony";
      extraGroups = [ "users" "wheel" "audio" "video" "vboxusers" ];
      useDefaultShell = true;
    }];
  };

  security = {
    sudo.configFile = ''
      root   ALL=(ALL) SETENV: ALL
      %wheel ALL=(ALL) SETENV: ALL
     '';
    setuidPrograms = [ "pmount" "pumount" ];
  };
}
