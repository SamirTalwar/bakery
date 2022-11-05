#!/usr/bin/env bake

import Bakery

main = bake do
  input <- existing $ file "input.file"

  target <- recipe (file "output.file") $ \output ->
    readF input |> run (cmd "tr" ~ "[:lower:]" ~ "[:upper:]") |> writeF output

  recipe (file "again.file") $ \output ->
    readF target |> run (cmd "tr" ~ "[:upper:]" ~ "[:lower:]") |> run (cmd "tr" ~ "[:lower:]" ~ "[:upper:]") |> writeF output

  defaultRecipe target
