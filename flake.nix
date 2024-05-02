{
  description = "clickhouse-driver";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs @ {
    self,
    flake-parts,
    nixpkgs,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = {self', ...}: {
        haskellProjects.default = {
          settings = {
            cassava-conduit = {
              broken = false;
              check = false;
            };
          };
        };
        packages.default = self'.packages.clickhouse-driver;
      };
    };
}
