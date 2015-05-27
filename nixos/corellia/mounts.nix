{ config, pkgs, ... }:

{
  # To install dependencies on those filesystems
  boot.initrd.supportedFilesystems = [ "vfat" "ntfs" "cifs" "nfs" ];

  system.activationScripts.media =
  ''
    mkdir -m 0755 -p /media /share
  '';

  # fileSystems."/share" = {
  #   device = "//nas/share";
  #   fsType = "cifs";
  # };

  # fileSystems."/share" = {
  #  device = "nas:/volume1/share";
  #  fsType = "nfs";
  #};
}
