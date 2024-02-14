{
  description = "OrgSelf";
  inputs = {
    ema.url = "github:srid/ema";
    nixpkgs.follows = "ema/nixpkgs";

    org-mode = {
      url = "github:fosskers/org-mode/master";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
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
              ema = disableCabalFlag inputs.ema.defaultPackage.${system} "with-examples";
              lvar = self.callCabal2nix "lvar" inputs.ema.inputs.lvar { }; # Until lvar gets into nixpkgs
              org-mode = dontCheck (self.callCabal2nix "org-mode" (inputs.org-mode + /org-mode) { });
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
        packages.default = project false;

        # Used by `nix develop`
        devShells.default = project true;
      });
}
