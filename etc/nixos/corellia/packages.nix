{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    pgadmin
    python3 python34Packages.pip python34Packages.flake8 python34Packages.pygit2
    # texLiveFull
    mysql mysqlWorkbench
    skype
    openvpn networkmanager_openvpn
  ];
}
