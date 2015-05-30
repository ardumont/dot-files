{ config, pkgs, ... }:

{
  # fileSystems."/share" = {
  #   device = "//nas/share";
  #   fsType = "cifs";
  # };

  fileSystems."/share" = {
   device = "nas:/volume1/share";
   fsType = "nfs";
  };
}
