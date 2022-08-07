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

      examples = builtins.attrNames (pkgs.lib.attrsets.filterAttrs (name: value: value == "directory") (builtins.readDir ./examples));
    in
    rec {
      packages.lib = haskellPackages.callCabal2nix name ./lib { };
      packages.ghcWithLib = haskellPackages.ghcWithPackages (_: [ packages.lib ]);
      packages.bake = pkgs.writeShellScriptBin "bake" ''
        exec ${packages.ghcWithLib}/bin/runhaskell \
          -XBlockArguments \
          -XExtendedDefaultRules \
          -XImportQualifiedPost \
          "$@"
      '';
      packages.default = packages.bake;

      packages.examples = haskellPackages.callCabal2nix "${name}-examples" ./examples { bakery = packages.lib; };

      apps.bake = flake-utils.lib.mkApp {
        drv = packages.bake;
        exePath = "/bin/bake";
      };
      apps.default = apps.bake;

      # Lets you run the examples with Nix.
      # For example, `nix run .#examples.echo` will run the "echo" example.
      apps.examples = pkgs.lib.attrsets.genAttrs examples (name:
        flake-utils.lib.mkApp {
          drv = packages.examples;
          exePath = "/bin/${name}";
        }
      );

      formatter = pkgs.nixpkgs-fmt;

      devShells.default = with pkgs; mkShell {
        buildInputs = [
          haskellCompiler
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.hlint
          haskellPackages.hpack
          haskellPackages.ormolu
        ];
      };
    }
    );
}
