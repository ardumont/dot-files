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

     # default haskell environment to provide when dev (install: nix-env -iA nixos.pkgs.hsEnv)
     hsEnv = self.haskellPackages.ghcWithPackages (pack : [
         pack.cabalInstall
         pack.cabal2nix
         pack.lens
         pack.hlint
         pack.hdevtools
         pack.zlib
         pack.mtl
         pack.HUnit
         pack.QuickCheck
         # pack.Agda
      ]);

    xmonadHsEnv = self.haskellPackages.ghcWithPackages (pack : [
       pack.xmonad
       pack.xmonadContrib
       pack.xmonadExtras
       pack.xmobar
     ]);

   };
 }