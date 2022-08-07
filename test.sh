#!/usr/bin/env bash
# Requires the `nix develop` environment.
# Should eventually be replaced by a bake.hs file itself.

set -e
set -u
set -o pipefail

haskell_packages=(lib examples)
haskell_files=(lib/**/*.hs examples/**/*.hs)

function note {
  echo '+' "$@"
}

for package in "${haskell_packages[@]}"; do
  note "hpack ${package}"
  hpack "$package"
done

note 'nix build .#bake'
nix build '.#bake' --out-link out/bake

note 'smoke examples/smoke.yaml'
nix shell '.#bake' --command smoke examples/smoke.yaml

note 'hlint'
hlint "${haskell_files[@]}"

note 'ormolu'
ormolu --mode=check "${haskell_files[@]}"
