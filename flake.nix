{
  description = "The Bakery";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;

    nixpkgs.url = github:NixOS/nixpkgs/master;
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
    let
      name = "bakery";

      ghcVersion = "9.2.3";
      ghcVersionString = "ghc" + builtins.replaceStrings [ "." ] [ "" ] ghcVersion;
    in
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      haskellCompiler = pkgs.haskell.compiler."${ghcVersionString}";
      haskellPackages = pkgs.haskell.packages."${ghcVersionString}";
    in
    rec {
      packages.lib = haskellPackages.callCabal2nix name ./lib { };
      packages.default = packages.lib;

      formatter = pkgs.nixpkgs-fmt;

      devShells.default = with pkgs; mkShell {
        buildInputs = [
          haskellCompiler
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.hpack
          haskellPackages.ormolu
        ];
      };
    }
    );
}
