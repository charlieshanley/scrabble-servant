let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.callPackage ./scrabble-servant.nix { }

