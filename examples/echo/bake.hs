#!/usr/bin/env bake

import Bakery

main = bake do
  recipe (file "output.file") $ \output ->
    run "echo" "Hello," "world!" |> writeF output
