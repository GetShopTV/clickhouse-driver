{
  description = "clickhouse-driver";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";

    haskell-flake.url = "github:srid/haskell-flake";

    # Same revision as the `source-repository-package` pin in cabal.project.
    hcurl = {
      url = "github:Reykudo/hcurl/b9b16d6f1f676904ce5fd70ad5384f3d144681a3";
      flake = false;
    };
  };

  outputs =
    inputs@{ self, ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;

      imports = [
        inputs.haskell-flake.flakeModule
      ];

      perSystem =
        {
          config
          , pkgs
          , self'
          , ...
        }:
        let
          # libcurl/libuv are C libraries; the dev shell needs both headers
          # (.dev), shared libraries (.out) and zlib's .pc file.
          curlUvDev = [
            pkgs.curl.dev
            pkgs.curl.out
            pkgs.libuv.dev
            pkgs.libuv.out
            pkgs.zlib.dev
            pkgs.zlib.out
          ];
        in
        {
          haskellProjects.default = {
            autoWire = [
              "packages"
              "checks"
            ];

            # hcurl is not on Hackage, so it comes from the flake input and
            # enters the Haskell package set through cabal2nix.
            packages.hcurl = {
              source = inputs.hcurl;
              cabalFlags.no-pkg-config = true;
            };

            settings.hcurl = {
              check = false;
              extraBuildTools = [
                pkgs.haskellPackages.c2hs
              ];
              # Wire libcurl/libuv into the hcurl build (the -fno-pkg-config
              # flag makes cabal link via extra-libraries instead).
              librarySystemDepends = curlUvDev;
            };
          };

          packages.default = self'.packages.clickhouse-driver;

          devShells.default = pkgs.mkShell {
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];

            packages = curlUvDev ++ [
              pkgs.pkg-config
              pkgs.haskellPackages.c2hs
              pkgs.git
            ];

            # For cabal builds inside the shell: pkg-config paths are set up
            # by nixpkgs, but GHC needs to find -lcurl/-luv when linking.
            env = {
              LIBRARY_PATH = "${pkgs.curl.out}/lib:${pkgs.libuv.out}/lib";
              LD_LIBRARY_PATH = "${pkgs.curl.out}/lib:${pkgs.libuv.out}/lib";
            };
          };
        };
    };
}
