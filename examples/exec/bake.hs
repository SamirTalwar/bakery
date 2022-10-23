#!/usr/bin/env bake

import Bakery

main = bake do
  recipe (exec "true") do
    run_ "true"

  recipe (exec "false") do
    run_ "false"

  recipe (exec "count") do
    run_ "seq" 1 10
