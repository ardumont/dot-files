{config, pkgs, ...}:

{
  # https://nixos.org/wiki/Printers
  services.printing = {
    enable = true;
    drivers = [
      pkgs.gutenprint
      pkgs.hplip
    ];
  };

  hardware.sane = {
    enable = true;
    # Support for HP scanners
    extraBackends = [ pkgs.hplipWithPlugin ];
  };
}
