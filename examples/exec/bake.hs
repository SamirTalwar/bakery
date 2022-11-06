#!/usr/bin/env bake

import Bakery

main = bake do
  recipe (exec "true") $ shell do
    n $ run (cmd "true")

  recipe (exec "false") $ shell do
    n $ run (cmd "false")

  recipe (exec "count") $ shell do
    n $ run (cmd "seq" ~ 1 ~ 10)
