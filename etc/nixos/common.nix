{ config, pkgs, ... }:

{
  system.autoUpgrade.enable = true;

  system.activationScripts.media =
  ''
    mkdir -m 0775 -p /volume/
    mkdir -m 0775 -p /volume/share
    chown -R root:users /volume/
  '';

  boot = {
    # To install dependencies on those filesystems
    initrd.supportedFilesystems = [ "vfat" "ntfs" "cifs" "nfs" ];

    # dropbox setting
    kernel.sysctl."fs.inotify.max_user_watches" = 1000000;

    loader = {
      # Use the systemd-boot EFI boot loader.
      systemd-boot.enable = true;
      efi = {
        canTouchEfiVariables = true;

        # assuming /boot is the mount point of the EFI partition in
        # NixOS (as the installation section recommends).
        efiSysMountPoint = "/boot";
      };
    };
  };

  time.timeZone = "Europe/Paris";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

  fonts = {
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
}
