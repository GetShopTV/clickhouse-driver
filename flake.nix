{
  description = "clickhouse-driver";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    cassava-conduit = {
      url = "github:domdere/cassava-conduit";
      flake = false;
    };
  };

  outputs = inputs@{ self, flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', inputs', system, config, final, lib, pkgs, ... }:
        {
          haskellProjects.default = {
            basePackages = pkgs.haskell.packages.ghc926;
            overrides = with pkgs.haskell.lib; hfinal: hprev: {
              cassava-conduit = lib.pipe (hfinal.callCabal2nix "cassava-conduit" inputs.cassava-conduit { })
                [ dontCheck ];
            };
          };
          packages.default = config.packages.clickhouse-driver;
        };
    };
}
