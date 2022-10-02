{
  description = "The Bakery";

  inputs = {
    nixpkgs = {
      url = github:NixOS/nixpkgs/master;
    };

    flake-utils = {
      url = github:numtide/flake-utils;
    };

    smoke = {
      url = github:SamirTalwar/smoke/99770bfd;
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , smoke
    }:
    let
      name = "bakery";

      ghcVersion = "9.2.4";
      ghcVersionString = "ghc" + builtins.replaceStrings [ "." ] [ "" ] ghcVersion;
    in
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };
      haskellCompiler = pkgs.haskell.compiler.${ghcVersionString};
      haskellPackages = pkgs.haskell.packages.${ghcVersionString}.override {
        overrides = hself: hsuper:
          let
            # Ormolu v0.5.0.1 doesn't build correctly on aarch64-darwin.
            # Disabling the "fixity-th" flag seems to fix it.
            # https://github.com/tweag/ormolu/issues/927
            fixOrmolu = p: pkgs.lib.pipe p [
              (pkgs.haskell.lib.compose.addExtraLibrary hself.file-embed)
              (pkgs.haskell.lib.compose.disableCabalFlag "fixity-th")
            ];
          in
          {
            ormolu = hself.ormolu_0_5_0_1;
            ormolu_0_5_0_1 = fixOrmolu hsuper.ormolu_0_5_0_1;
            fourmolu = hself.fourmolu_0_8_2_0;
            fourmolu_0_8_2_0 = fixOrmolu hsuper.fourmolu_0_8_2_0;
          };
      };

      examples = builtins.attrNames (pkgs.lib.attrsets.filterAttrs (name: value: value == "directory") (builtins.readDir ./examples));
    in
    rec {
      packages.core = haskellPackages.callCabal2nix name ./core { };
      packages.lib = haskellPackages.callCabal2nix name ./lib {
        bakery-core = packages.core;
        bakery-shell = packages.shell;
      };
      packages.shell = haskellPackages.callCabal2nix name ./shell {
        bakery-core = packages.core;
      };
      packages.ghcWithLib = haskellPackages.ghcWithPackages (_: [ packages.lib ]);
      packages.bake = pkgs.stdenv.mkDerivation {
        name = "bake";
        src = ./.;
        buildPhase = ''
          (
            echo '#!${pkgs.bash}/bin/bash'
            echo
            echo 'PATH="${packages.ghcWithLib}/bin:''${PATH}"'
            tail -n+2 bin/bake
          ) > bake
          chmod +x bake
        '';
        installPhase = ''
          mkdir -p $out/bin
          mv bake $out/bin/
        '';
      };
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
          haskellPackages.hspec-discover
          haskellPackages.ormolu
          smoke.outputs.packages."${system}".default
        ];
      };
    }
    );
}
