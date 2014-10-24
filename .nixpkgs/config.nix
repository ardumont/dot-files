{ pkgs }:

{
  packageOverrides = self : with pkgs; rec {
    # dummy env to understand nix-env - https://nixos.org/wiki/Howto_develop_software_on_nixos
    someEnvNameEnv = self.myEnvFun {
        name = "someEnvName";
        buildInputs = [ stdenv pkgconfig ];
    };

    sdlEnv = self.myEnvFun {
        name = "sdl";
        buildInputs = [ stdenv SDL SDL_image SDL_ttf SDL_gfx cmake SDL_net pkgconfig ];
    };

    # default haskell environment to provide when dev
    # install: nix-env -iA nixos.pkgs.hsEnv
    # load: load-hsEnv
    hsEnv = self.myEnvFun {
      name = "haskell";
      buildInputs = [
        haskellPackages.cabalInstall
        haskellPackages.cabal2nix
        haskellPackages.lens
        haskellPackages.hlint
        haskellPackages.hdevtools
        haskellPackages.zlib
        haskellPackages.mtl
        haskellPackages.HUnit
        haskellPackages.QuickCheck
        # haskellPackages.Agda
      ];
    };

    # default purescript environment
    # install: nix-env -i env-purescript
    # load: load-env-purescript
    purescriptEnv = self.myEnvFun {
      name = "purescript";
      buildInputs = [
        haskellPackages.purescript
        nodejs
        nodePackages.npm
        nodePackages.jshint
        nodePackages.grunt-cli
        nodePackages.bower
      ];
    };

    # default xmonad env
    # install: nix-env -i env-xmonad
    # load: load-env-xmonad
    xmonadEnv = self.myEnvFun {
      name = "xmonad";
      buildInputs = [
       haskellPackages.xmonad
       haskellPackages.xmonadContrib
       haskellPackages.xmonadExtras
       haskellPackages.xmobar
      ];
    };

  };
}
