#!/usr/bin/env bake

import Bakery

main = bake do
  input <- existing $ file "input.file"

  main <- recipe (file "output.file") $ shell do
    readF input |> run (cmd "tr" ~ "[:lower:]" ~ "[:upper:]") |> writeF target

  recipe (file "again.file") $ shell do
    readF main |> run (cmd "tr" ~ "[:upper:]" ~ "[:lower:]") |> run (cmd "tr" ~ "[:lower:]" ~ "[:upper:]") |> writeF target

  defaultRecipe main
