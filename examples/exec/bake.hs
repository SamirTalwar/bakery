#!/usr/bin/env bake

import Bakery

main = bake do
  recipe (exec "true") do
    run "true"

  recipe (exec "false") do
    run "false"

  recipe (exec "count") do
    run "seq" 1 10
