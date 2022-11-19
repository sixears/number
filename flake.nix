{
  description = "More classes & utilities for working with numbers";

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = github:sixears/flake-build-utils/r1.0.0.13;
  };

  outputs = { self, nixpkgs, build-utils }:
    build-utils.lib.hOutputs self nixpkgs "number" {
      ghc = p: p.ghc8107; # for tfmt
      callPackage = { mkDerivation, lib, mapPkg, system
                    , base, base-unicode-symbols }:
        let
          pkg = build-utils.lib.flake-def-pkg system;
        in
          mkDerivation {
            pname = "number";
            version = "1.1.2.10";
            src = ./.;
            libraryHaskellDepends = [ base base-unicode-symbols ];
            description = "manage info.yaml";
            license = lib.licenses.mit;
          };
    };
}
