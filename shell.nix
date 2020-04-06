{ pkgs ? import <nixpkgs> {} }:

with pkgs;
mkShell {
  name = "bakery";

  buildInputs = [
    cargo
    rustc
    rustfmt
    rustracer
  ];
}
