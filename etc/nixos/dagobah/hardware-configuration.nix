# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];

  boot.initrd.availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" "sd_mod" ];
  boot.kernelModules = [ "acpi-cpufreq" "kvm-intel" ];
  boot.extraModulePackages = [ ];

  hardware.opengl.enable = true;

  fileSystems."/" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-label/boot";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/swap"; }
    ];

  nix.maxJobs = 4;
  hardware.bluetooth.enable = false;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
