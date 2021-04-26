{
  description = "OrgSelf";
  inputs = {
    ema.url = "github:srid/ema/48c92c3204c6ddaaf1f522e4a3d83b92ae3eb723";
    # FIXME: uncommenting this gives,
    #   error: in pure evaluation mode, 'fetchTarball' requires a 'sha256' argument
    # nixpkgs.follows = "ema";

    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [ ];
        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowBroken = true; # To allow `org-mode` package which is broken
        };
        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "orgself";
            root = ./.;
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              ema = dontHaddock (inputs.ema.defaultPackage.${system});
              org-mode = dontCheck super.org-mode; # `tasty` dependency is broken on nixpkgs
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
              [
                cabal-install
                cabal-fmt
                pkgs.nixpkgs-fmt
                ghcid
                ormolu
                haskell-language-server
              ]);
          };
      in
      {
        # Used by `nix build`
        defaultPackage = project false;

        # Used by `nix develop`
        devShell = project true;
      });
}
