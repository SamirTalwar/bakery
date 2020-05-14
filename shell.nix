{ pkgs ? import <nixpkgs> {}
, smoke ? (import (pkgs.fetchFromGitHub {
    owner = "SamirTalwar";
    repo = "smoke";
    rev = "944ec824a5906b6bf910289a4e1c382cdd6e8324";
    sha256 = "1rv7q2i086i1k5kfscm97w4zlg04i60ws5z1pm8q907grydp2sgf";
  }) {}).smoke
}:

with pkgs;
mkShell {
  name = "bakery";

  buildInputs = [
    cargo
    rustc
    rustfmt
    rustracer
    smoke
  ] ++ (if stdenv.isDarwin then [darwin.apple_sdk.frameworks.Security] else []);
}
