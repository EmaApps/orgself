{
  description = "Memoir";
  inputs = {
    ema.url = "github:srid/ema/2d87a984fba03c2d1fb4a501641e3ccb2aa80753";
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
            name = "memoir";
            root = ./.;
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              ema = inputs.ema.defaultPackage.${system};
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
