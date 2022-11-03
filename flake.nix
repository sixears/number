{
  description = "More classes & utilities for working with numbers";

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = "github:sixears/flake-build-utils/r1.0.0.6";
  };

  outputs = { self, nixpkgs, flake-utils, build-utils }:
    build-utils.lib.hOutputs self nixpkgs "number" {
      ghc = p: p.ghc8107; # for tfmt
    };
}
