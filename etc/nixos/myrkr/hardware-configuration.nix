# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, ... }:

{
  imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];

  # Do not activate if not needed
  nixpkgs.config = {
    allowUnfree = true;
    # allowBroken = true;
  };

  boot.initrd.availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" "usbhid" ];
  networking.enableB43Firmware = false;        # don't want those, they conflict with wl (and do not work)
  boot.blacklistedKernelModules = [ "ath9k" "b43" "bcma" ];  # force the blacklist on  wifi modules
  boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];  # wl module for wifi
  boot.kernelModules = [ "acpi-cpufreq" "kvm-intel" "wl" ];

  fileSystems."/" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-label/volume";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-label/swap"; }
    ];

  nix.maxJobs = 8;
  hardware.bluetooth.enable = false;
}
