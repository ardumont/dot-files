{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    python3
    python36
    python36Packages.virtualenvwrapper python36Packages.virtualenv
    # texLiveFull
    docker docker_compose
    openvpn networkmanager-openvpn
    git
  ];
}
