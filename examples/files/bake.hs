#!/usr/bin/env bake

import Bakery

main = bake do
  thisDirectory <- currentDirectory

  existing <- existing $ file "existing.file"

  one <- recipe (file "one.file") $ shell do
    run (cmd "echo" ~ "one") |> writeF target

  two <- recipe (file "two.file") $ shell do
    run (cmd "echo" ~ "two") |> writeF target

  three <- recipe (file "three.file") $ shell do
    run (cmd "echo" ~ "three") |> writeF target

  recipe (file "cat-existing.file") $ shell do
    run (cmd "cat" ~ existing ~ one ~ two) |> writeF target

  cat <- recipe (file "cat.file") $ shell do
    run (cmd "cat" ~ one ~ two) |> writeF target

  recipe (file "cat-more.file") $ shell do
    run (cmd "cat" ~ cat ~ three) |> writeF target

  recipe (file "copy.file") $ shell do
    run (cmd "cp" ~ existing ~ target)

  recipe (file "two-phase.file") $ shell do
    let firstPhase = thisDirectory <> "/../tmp/first-phase.file"
    run (cmd "echo" ~ "one" ~ "two") |> writeF firstPhase
    n $ run (cmd "cp" ~ firstPhase ~ target)

  recipe (file "non-existent.file") $ shell do
    run (cmd "cat" ~ one)

  recipe (file "../files/./indirect.file") $ shell do
    run (cmd "echo" ~ "this got there eventually") |> writeF target

  recipe (file $ thisDirectory <> "/../tmp/output.file") $ shell do
    run (cmd "echo" ~ "This is a test.") |> writeF target

  recipe (file $ thisDirectory <> "/../tmp/non-existent.file") $ shell do
    run (cmd "cat")
