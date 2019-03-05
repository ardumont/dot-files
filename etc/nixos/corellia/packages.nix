{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    python3
    mr
    python36
    python36Packages.virtualenvwrapper python36Packages.virtualenv
    python36Packages.cffi python36Packages.vcversioner
    python36Packages.pystemd python36Packages.systemd
    postgresql pkgconfig
    # texLiveFull
    docker docker_compose
    openvpn
  ];
}
