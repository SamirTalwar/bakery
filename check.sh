#!/usr/bin/env nix-shell
#!nix-shell --pure -i zsh shell.nix

set -e
set -u

echo >&2 '> cargo test'
cargo test

echo >&2 '> smoke spec'
smoke spec
echo >&2

echo >&2 '> cargo clippy'
cargo clippy --tests -- \
  -D clippy::all \
  -D clippy::pedantic \
    -A clippy::default-trait-access \
    -A clippy::match_wildcard_for_single_variants \
    -A clippy::wildcard_imports
echo >&2

echo >&2 '> rustfmt'
rustfmt --check src/**/*.rs
echo >&2

echo >&2 '> nixpkgs-fmt'
nixpkgs-fmt --check *.nix nix/*.nix
echo >&2
