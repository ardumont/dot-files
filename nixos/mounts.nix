{ config, pkgs, ... }:

{
  # fileSystems."/share" = {
  #   device = "//nas/share";
  #   fsType = "cifs";
  # };

  fileSystems."/share" = {
   device = "naboo:/volume1/share";
   fsType = "nfs";
  };
}
