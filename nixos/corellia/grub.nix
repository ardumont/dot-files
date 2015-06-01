{ config, pkgs, ... }:

{
  # Add debian entry in nixos's grub bootloader
  boot.loader.grub = {
    extraEntries = ''
        menuentry "debian 8" {
          # set root 'hd0, msdos1'
          # linux /boot/vmlinuz-3.16.0-4-amd64 ro quiet persistent
          # init /boot/initrd.img-3.16.0-4-amd64

          search --set=drive1 --fs-uuid 8af90e9e-cb5d-491b-89f7-d39fa58051fa
          linux ($drive1)/boot/vmlinuz-3.16.0-4-amd64 ro quiet persistent
          initrd ($drive1)/boot/initrd.img-3.16.0-4-amd64
        }
      '';
    extraEntriesBeforeNixOS = true; # before the default option
  };

}
