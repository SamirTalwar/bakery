#!/usr/bin/env bake

import Bakery

main = bake do
  input <- existing $ file "input.file"

  recipe (file "output.file") $ \output ->
    readF input |> run "tr" "[:lower:]" "[:upper:]" |> writeF output
