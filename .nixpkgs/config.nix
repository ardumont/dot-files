{ pkgs }:

{
  # source: https://nixos.org/wiki/Howto_develop_software_on_nixos
  packageOverrides = self : with pkgs; with sourceAndTags;
    # default dev environment with common needed tools
    let defaultDevEnv = { name, buildInputs ? [], cTags ? [], extraCmds ? ""}:
      pkgs.myEnvFun {
        inherit name;
        shell = "/var/run/current-system/sw/bin/zsh";
        buildInputs = buildInputs
          ++ map (x : sourceWithTagsDerivation ( (addCTaggingInfo x ).passthru.sourceWithTags ) ) cTags
          ++ [ gitFull zsh keychain ];
        extraCmds = ''
          HOME=${builtins.getEnv "HOME"}
          ${extraCmds}
        '';

    };
    in rec {

      # install: nix-env -i env-sdl
      # load: load-env-sdl
      sdlEnv = self.myEnvFun {
          name = "sdl";
          buildInputs = [ stdenv SDL SDL_image SDL_ttf SDL_gfx cmake SDL_net pkgconfig ];
      };

      # default haskell environment to provide when dev
      # install: nix-env -iA nixos.pkgs.haskell
      #          nix-env -i env-haskell
      # load: load-env-haskell
      hsEnv = defaultDevEnv {
        name = "haskell";
        buildInputs = with pkgs.haskellPackages; [
          cabalInstall
          cabal2nix
          lens
          hlint
          hdevtools
          zlib
          mtl
          HUnit
          QuickCheck
          # Agda
        ];
      };

      # default purescript environment
      # install: nix-env -i env-purescript
      # load: load-env-purescript
      purescriptEnv = defaultDevEnv {
        name = "purescript";
        buildInputs = [
          haskellPackages.purescript
          nodejs
          nodePackages.npm
          nodePackages.npm2nix
          nodePackages.jshint
          nodePackages.grunt-cli
          nodePackages.bower
        ];
      };

      # default xmonad env
      # install: nix-env -i env-xmonad
      # load: load-env-xmonad
      xmonadEnv = defaultDevEnv {
        name = "xmonad";
        buildInputs = with pkgs.haskellPackages; [
         xmonad
         xmonadContrib
         xmonadExtras
         xmobar
        ];
      };

      # default mynodejs environment
      # install: nix-env -i env-mynodejs
      # load: load-env-mynodejs
      myNodeJSEnv = defaultDevEnv {
        name = "mynodejs";
        buildInputs = [
          nodejs
          nodePackages.npm
          nodePackages.npm2nix
          nodePackages.jshint
          nodePackages.grunt-cli
          nodePackages.bower
        ];
      };
   };
}
