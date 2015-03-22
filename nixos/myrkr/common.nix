{ config, pkgs, ... }:

{
  boot = {
    loader.grub = {
      enable = true;           # Use GRUB boot loader.
      version = 2;             # Version 2
      device = "/dev/sda";     # which hard drive to install it
      memtest86.enable = true; # Activate the check on memory
    };

    # sound options
    extraModprobeConfig = ''
      options snd slots=snd-hda-intel
# to select the second card by default
#      options snd_hda_intel enable=0,1
    '';

    # dropbox setting
    kernel.sysctl."fs.inotify.max_user_watches" = 1000000;
  };

  time.timeZone = "Europe/Paris";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

  programs.ssh.startAgent = false; # do not start ssh agent (gpg-agent will do)

  # List services that you want to enable:
  services = {
    acpid.enable = true;

    locate = {
      enable = true;
      period = "00 19 * * *"; # update db at 19h every day
    };

    openssh.enable = true;
    ntp.enable = true;

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
    fontconfig.enable = true;
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

  security = {
    sudo.configFile = ''
      root   ALL=(ALL) SETENV: ALL
      %wheel ALL=(ALL) SETENV: ALL
     '';
    setuidPrograms = [ "pmount" "pumount" ];
  };
}
