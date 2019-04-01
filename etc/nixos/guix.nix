{ config, pkgs, ... }:

{
  # taken from the guix-installer step failure
  systemd.services.guix-daemon =  {
    # This is a "service unit file" for the systemd init system to launch
    # 'guix-daemon'.  Drop it in /etc/systemd/system or similar to have
    # 'guix-daemon' automatically started.
    description = "Build daemon for GNU Guix";
    serviceConfig = {
      Environment = "GUIX_LOCPATH=/var/guix/profiles/per-user/root/guix-profile/lib/locale";
      ExecStart = "/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon --build-users-group=guixbuild";
      RemainAfterExit = true;
      StandardOutput = "syslog";
      StandardError = "syslog";
      # See <https://lists.gnu.org/archive/html/guix-devel/2016-04/msg00608.html>.
      # Some package builds (for example, go@1.8.1) may require even more than
      # 1024 tasks.
      TasksMax = 8192;
    };
    wantedBy = [ "multi-user.target" ];
    wants = [ "network.target" ];
  };

}
