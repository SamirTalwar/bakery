#!/usr/bin/env bake

import Bakery

main = bake do
  thisDirectory <- currentDirectory

  existing <- existing $ file "existing.file"

  one <- recipe (file "one.file") do
    n $ run (cmd "echo" ~ "one") |> writeF target

  two <- recipe (file "two.file") do
    n $ run (cmd "echo" ~ "two") |> writeF target

  three <- recipe (file "three.file") do
    n $ run (cmd "echo" ~ "three") |> writeF target

  recipe (file "cat-existing.file") do
    n $ run (cmd "cat" ~ existing ~ one ~ two) |> writeF target

  cat <- recipe (file "cat.file") do
    n $ run (cmd "cat" ~ one ~ two) |> writeF target

  recipe (file "cat-more.file") do
    n $ run (cmd "cat" ~ cat ~ three) |> writeF target

  recipe (file "copy.file") do
    n $ run (cmd "cp" ~ existing ~ target)

  recipe (file "two-phase.file") $ do
    let firstPhase = thisDirectory <> "/../tmp/first-phase.file"
    n $ run (cmd "echo" ~ "one" ~ "two") |> writeF firstPhase
    n $ run (cmd "cp" ~ firstPhase ~ target)

  recipe (file "non-existent.file") do
    n $ run (cmd "cat" ~ one)

  recipe (file "../files/./indirect.file") do
    n $ run (cmd "echo" ~ "this got there eventually") |> writeF target

  recipe (file $ thisDirectory <> "/../tmp/output.file") do
    n $ run (cmd "echo" ~ "This is a test.") |> writeF target

  recipe (file $ thisDirectory <> "/../tmp/non-existent.file") do
    n $ run (cmd "cat")
