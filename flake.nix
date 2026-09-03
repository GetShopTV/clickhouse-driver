{
  description = "clickhouse-driver";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # Same revision as the `source-repository-package` pin in cabal.project;
    # kept as an input so the driver sources are easy to reference.
    hcurl.url = "github:Reykudo/hcurl/14a333c8e0a12b64ab6863e2c149f07d301c6649";
    hcurl.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , hcurl
    }:
    let
      systems = nixpkgs.lib.systems.flakeExposed;
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            name = "clickhouse-driver-dev";

            nativeBuildInputs = [
              pkgs.ghc
              pkgs.cabal-install
              pkgs.haskellPackages.c2hs
              pkgs.pkg-config
              pkgs.git
            ];

            # hcurl (built by cabal from cabal.project) binds libcurl and
            # libuv; the dev shell must expose headers, .pc files and the
            # shared libraries. pkg-config paths are set up automatically by
            # nixpkgs; LIBRARY_PATH/LD_LIBRARY_PATH are needed by the ambient
            # GHC when linking against -lcurl/-luv.
            buildInputs = [
              pkgs.curl.dev
              pkgs.curl.out
              pkgs.libuv.dev
              pkgs.libuv.out
              pkgs.zlib.dev
              pkgs.zlib.out
            ];

            env = {
              LIBRARY_PATH = "${pkgs.curl.out}/lib:${pkgs.libuv.out}/lib";
              LD_LIBRARY_PATH = "${pkgs.curl.out}/lib:${pkgs.libuv.out}/lib";
            };
          };
        }
      );
    };
}
