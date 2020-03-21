{ pkgs, ...}:

{
  # https://nixos.org/wiki/Printers
  services.printing = {
    enable = true;
    drivers = [
      pkgs.gutenprint
      (pkgs.lib.overrideDerivation pkgs.hplip (attrs: {
        # name = "hplip-3.15.4";
        # Support for HP scanners
        withPlugin = true;
      }))
    ];
  };

  hardware.sane = {
    enable = true;
    # extraBackends = [ pkgs.hplipWithPlugin ]; # no need because activated previously
  };
}
