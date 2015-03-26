{ pkgs,
  emacsPackagesNg ? (import <nixpkgs> {}).emacsPackagesNg }:

{
  # Allow broken content (when using nixpkgs as git)
  allowBroken = true;

  # Allow unfree license
  allowUnfree = true;

  # source: https://nixos.org/wiki/Howto_develop_software_on_nixos
  packageOverrides = self : with pkgs; with sourceAndTags;
    # default dev environment with common needed tools
    let defaultDevEnv = { name, buildInputs ? [], cTags ? [], extraCmds ? ""}:
          pkgs.myEnvFun {
            inherit name;
            shell = "/var/run/current-system/sw/bin/zsh";
            buildInputs = buildInputs
              ++ map (x : sourceWithTagsDerivation ( (addCTaggingInfo x ).passthru.sourceWithTags ) ) cTags
              ++ [ gitFull zsh keychain emacs tmux gnumake ];
            extraCmds = ''
              HOME=${builtins.getEnv "HOME"}
              ${extraCmds}
            '';
          };

        defaultJavaEnv = { name, buildInputs ? [] }:
          defaultDevEnv {
            inherit name;
            buildInputs = buildInputs ++ [ maven ant idea.idea-community jd-gui ];
          };

        composeDevEnv = { name, envs }:
          defaultDevEnv {
            inherit name;
            buildInputs = pkgs.lib.concatMap (env: env.buildInputs) envs;
          };

    in rec {

      # xmonad-with-packages = callPackage /home/tony/repo/perso/nixpkgs/pkgs/applications/window-managers/xmonad/wrapper.nix {
      #   ghcWithPackages = haskellngPackages.ghcWithPackages;
      #   packages = self: [ haskellPackages.xmonadContrib haskellPackages.xmonadExtras ];
      # };

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
          hoogle
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
          nodePackages.jshint
          nodePackages.bower
          nodePackages.grunt-cli
          nodePackages.npm2nix
          nodePackages.bower2nix
          nodePackages.phantomjs
          nodePackages.nodemon
          chromedriver
        ];
      };

      myAndroidEnv = defaultDevEnv {
        name = "myandroid";
        buildInputs = [
          androidsdk_4_4
          idea.android-studio
          idea.idea-community
          heimdall
        ];
      };

      javaEnv = defaultDevEnv {
        name = "java";
        buildInputs = [
          maven
          ant
          idea.idea-community
          jd-gui
        ];
      };

      java6Env = defaultJavaEnv {
        name = "java6";
        buildInputs = [ oraclejdk tomcat6 ];
      };

      java7Env = defaultJavaEnv {
        name = "java7";
        buildInputs = [ oraclejdk7 ];
      };

      java8Env = defaultJavaEnv {
        name = "java8";
        buildInputs = [ oraclejdk8 ];
      };

      cljEnv = defaultDevEnv {
        name = "clj";
        buildInputs = [ jdk clojure leiningen ];
      };

      emacslispEnv = defaultDevEnv {
        name = "emacslisp";
        buildInputs = [
          emacs
          emacs24Packages.cask
          python
        ];
      };

      wifiToolsEnv = defaultDevEnv {
        name = "wifiTools";
        buildInputs = [
          bridge_utils
          wirelesstools
          hostapd
        ];
      };

      proEnv = composeDevEnv {
        name = "pro";
        envs = [ myAndroidEnv java6Env myNodeJSEnv ];
      };

      jekyllEnv = defaultDevEnv {
        name = "static-site";
        buildInputs = with rubyLibs; [ jekyll bundler nokogiri ];
      };

      idrisEnv = defaultDevEnv {
        name = "idris";
        buildInputs = [ haskellPackages_ghc783_profiling.idris_plain ];
      };

      emacs = emacs24;

      emacsWithPackages = pkgs.buildEnv {
        ignoreCollisions = true;
        name = "emacs-with-packages";
#        buildInputs = [ mu ];
        paths = with emacsPackagesNg; [
          emacs24
          markdown-mode
          org-trello
          auto-complete
          ac-haskell-process
          company
          haskell-mode
          structured-haskell-mode
          ace-jump-mode
          exec-path-from-shell
          flycheck
          flycheck-pos-tip
          gnus
          god-mode
          aspell
          aspellDicts.en
          magit
          projectile
          switch-window
          smart-mode-line
          undo-tree
          use-package
          dash
          s
          deferred
          diminish
          popup

          # ag
          # auctex
          # change-inner
          # circe
          # expand-region
          # helm
          # helm-swoop
          # hi2
          # idris-mode
          # monokai-theme
          # org-plus-contrib
          # smartparens
          # volatile-highlights
          # wgrep
          # zenburn-theme
        ];
      };

   };
}
