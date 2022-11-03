#!/usr/bin/env bake

import Bakery

main = bake do
  recipe (exec "true") do
    n $ run (cmd "true")

  recipe (exec "false") do
    n $ run (cmd "false")

  recipe (exec "count") do
    n $ run (cmd "seq" 1 10)
