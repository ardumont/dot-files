{ ... }:

{
  fileSystems."/volume/share" = {
   device = "rpi3:/volume/share";
   fsType = "sshfs";
   options = [ "noauto" "noatime" "users" ];
  };
}
