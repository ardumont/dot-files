# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "job"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget curl
    gnumake
    xclip pass keychain
    htop 
    emacs texinfo
    dejavu_fonts xfontsel xlsfonts
    tmux rxvt_unicode
    git
    tcsh bash zsh python ruby
    firefox 
    haskellPackages.haskellPlatform
    haskellPackages.xmonad
    haskellPackages.xmobar
    haskellPackages.xmonadContrib
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    desktopManager.default = "none";
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e";

    # touchpad
    synaptics.enable = true;
    synaptics.twoFingerScroll = true;    

    # xkbOptions = "ctrl:nocaps,terminate=ctrl_alt_backspace";

    # Enable the KDE Desktop Environment.
    # displayManager.kdm.enable = true;
    # desktopManager.kde4.enable = true;

    windowManager = {
      default = "xmonad";
      xmonad.enable = true;
      xmonad.enableContribAndExtras = true;
      xmonad.extraPackages = haskellPackages: [
        haskellPackages.xmonadContrib  
      ];
    };
  };
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
      extraGroups = [ "users" "wheel" ];
      useDefaultShell = true;
    }];
  };
}
