#!/usr/bin/env bash
# Requires the `nix develop` environment.
# Should eventually be replaced by a bake.hs file itself.

set -e
set -u
set -o pipefail
shopt -s globstar

cd -- "$(dirname -- "${BASH_SOURCE[0]}")"

haskell_packages=(core lib shell examples)
haskell_files=()
for package in "${haskell_packages[@]}"; do
  haskell_files+=("$package"/**/*.hs)
done

if [[ -n "${SMOKE_TARGETS+x}" ]]; then
  # convert to array
  mapfile -t SMOKE_TARGETS <<< "$SMOKE_TARGETS"
else
  SMOKE_TARGETS=(examples/*/smoke.yaml)
fi

function note {
  echo '+' "$@"
}

for package in "${haskell_packages[@]}"; do
  note "hpack ${package}"
  hpack "$package"
done

note 'cabal build all'
cabal build -j all

note 'cabal test all'
cabal test -j all

note 'smoke'
(
  PATH="${PWD}/bin:${PATH}"
  cabal exec -- smoke "${SMOKE_TARGETS[@]}"
)

if [[ -n "${TEST_NIX+x}" ]]; then
  note 'nix build .#bake'
  nix build '.#bake' --out-link out/bake

  note 'smoke (with nix)'
  nix shell '.#bake' --command smoke examples/*/smoke.yaml
fi

note 'hlint'
hlint "${haskell_files[@]}"

note 'ormolu'
ormolu --mode=check "${haskell_files[@]}"
