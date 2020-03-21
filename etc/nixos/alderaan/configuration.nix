{ ... }:

{
  imports = [
    ./hardware-configuration.nix # Include the results of the hardware scan.
    ./xserver.nix
  ];
}
