{ config, pkgs, ... }:

{
  # Add debian entry in nixos's grub bootloader
  boot.loader.grub = {
    extraEntries = ''
        # retrieved from /boot/grub/grub.cfg after debian install
        function load_video {
          if [ x$feature_all_video_module = xy ]; then
            insmod all_video
          else
            insmod efi_gop
            insmod efi_uga
            insmod ieee1275_fb
            insmod vbe
            insmod vga
            insmod video_bochs
            insmod video_cirrus
          fi
        }

        menuentry "debian 8" {
          # not tested
          # set root='hd0, msdos1'
          # linux /boot/vmlinuz-3.16.0-4-amd64 ro quiet persistent
          # init /boot/initrd.img-3.16.0-4-amd64

          # not working, why?
          # search --set=drive1 --fs-uuid 8af90e9e-cb5d-491b-89f7-d39fa58051fa
          # linux ($drive1)/boot/vmlinuz-3.16.0-4-amd64 ro quiet persistent
          # initrd ($drive1)/boot/initrd.img-3.16.0-4-amd64

          load_video
          insmod gzio
          if [ x$grub_platform = xxen ]; then insmod xzio; insmod lzopio; fi
          insmod part_msdos
          insmod ext2
          set root='hd0,msdos1'
          if [ x$feature_platform_search_hint = xy ]; then
            search --no-floppy --fs-uuid --set=root --hint-bios=hd0,msdos1 --hint-efi=hd0,msdos1 --hint-baremetal=ahci0,msdos1  8af90e9e-cb5d-491b-89f7-d39fa58051fa
          else
            search --no-floppy --fs-uuid --set=root 8af90e9e-cb5d-491b-89f7-d39fa58051fa
          fi
          echo   'Loading Linux 3.16.0-4-amd64 ...'
          linux  /vmlinuz root=UUID=8af90e9e-cb5d-491b-89f7-d39fa58051fa ro persistent quiet
          echo   'Loading initial ramdisk ...'
          initrd /initrd.img
        }
      '';
    # extraEntriesBeforeNixOS = true; # before the default option
  };

}
