{ config, pkgs, ... }:

{
  imports = [
    ./git.nix
    ./gpg-agent.nix
    ./afew.nix
  ];

  home.packages = with pkgs; [
    htop fortune
  ];

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.nix-mode
      epkgs.magit
    ];
  };

  # programs.firefox = {
  #   enable = true;
  #   enableIcedTea = true;
  # };

  # Let Home Manager install and manage itself.
  programs.home-manager = {
    enable = true;
    # path = "…";
  };
}
