#!/usr/bin/env bake

import Bakery

main = bake do
  recipe (exec "true") $ shell do
    run (cmd "true")

  recipe (exec "false") $ shell do
    run (cmd "false")

  recipe (exec "count") $ shell do
    run (cmd "seq" ~ 1 ~ 10)
