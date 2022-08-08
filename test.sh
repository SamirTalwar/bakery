#!/usr/bin/env bash
# Requires the `nix develop` environment.
# Should eventually be replaced by a bake.hs file itself.

set -e
set -u
set -o pipefail
shopt -s globstar

haskell_packages=(core lib shell examples)
haskell_files=()
for package in "${haskell_packages[@]}"; do
  haskell_files+=("$package"/**/*.hs)
done

function note {
  echo '+' "$@"
}

for package in "${haskell_packages[@]}"; do
  note "hpack ${package}"
  hpack "$package"
done

note 'cabal build all'
cabal build all

note 'nix build .#bake'
nix build '.#bake' --out-link out/bake

note 'smoke examples/smoke.yaml'
nix shell '.#bake' --command smoke examples/smoke.yaml

note 'hlint'
hlint "${haskell_files[@]}"

note 'ormolu'
ormolu --mode=check "${haskell_files[@]}"
