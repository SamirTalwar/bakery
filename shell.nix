{ pkgs ? import <nixpkgs> {}
, smoke ? import (pkgs.fetchFromGitHub {
    owner = "SamirTalwar";
    repo = "smoke";
    rev = "eec4dd710a3fff495c268f6c7c606f21ab625ed6";
    sha256 = "01qsnicj12f9nv07fd75v3n4xmj4dzgcb3fv2700q3583zxxnkk6";
  }) {}
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
