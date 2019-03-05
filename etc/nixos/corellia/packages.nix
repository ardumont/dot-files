{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    python3
    # texLiveFull
    # docker
    openvpn networkmanager-openvpn
    git
  ];
}
