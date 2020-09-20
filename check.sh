#!/usr/bin/env nix-shell
#!nix-shell -i zsh shell.nix

set -e
set -u

echo >&2 '> cargo test'
cargo test

echo >&2 '> smoke spec'
smoke spec

echo >&2 '> cargo clippy'
cargo clippy --tests -- -D clippy::all

echo >&2 '> rustfmt'
rustfmt --check src/**/*.rs

echo >&2 '> nixpkgs-fmt'
nixpkgs-fmt --check *.nix nix/*.nix
