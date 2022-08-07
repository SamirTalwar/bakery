#!/usr/bin/env bash
# Requires the `nix develop` environment.
# Should eventually be replaced by a bake.hs file itself.

set -e
set -u
set -o pipefail
set -x

nix build '.#bake' --out-link out/bake
nix shell '.#bake' --command smoke examples/smoke.yaml
