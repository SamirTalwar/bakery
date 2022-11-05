#!/usr/bin/env bake

import Bakery

main = bake do
  thisDirectory <- currentDirectory

  existing <- existing $ file "existing.file"

  one <- recipe (file "one.file") $ \output ->
    n $ run (cmd "echo" ~ "one") |> writeF output

  two <- recipe (file "two.file") $ \output ->
    n $ run (cmd "echo" ~ "two") |> writeF output

  three <- recipe (file "three.file") $ \output ->
    n $ run (cmd "echo" ~ "three") |> writeF output

  recipe (file "cat-existing.file") $ \output ->
    n $ run (cmd "cat" ~ existing ~ one ~ two) |> writeF output

  cat <- recipe (file "cat.file") $ \output ->
    n $ run (cmd "cat" ~ one ~ two) |> writeF output

  recipe (file "cat-more.file") $ \output ->
    n $ run (cmd "cat" ~ cat ~ three) |> writeF output

  recipe (file "copy.file") $ \output ->
    n $ run (cmd "cp" ~ existing ~ output)

  recipe (file "two-phase.file") $ \output -> do
    let firstPhase = thisDirectory <> "/../tmp/first-phase.file"
    n $ run (cmd "echo" ~ "one" ~ "two") |> writeF firstPhase
    n $ run (cmd "cp" ~ firstPhase ~ output)

  recipe (file "non-existent.file") $ \output ->
    n $ run (cmd "cat" ~ one)

  recipe (file "../files/./indirect.file") $ \output ->
    n $ run (cmd "echo" ~ "this got there eventually") |> writeF output

  recipe (file $ thisDirectory <> "/../tmp/output.file") $ \output ->
    n $ run (cmd "echo" ~ "This is a test.") |> writeF output

  recipe (file $ thisDirectory <> "/../tmp/non-existent.file") $ \output ->
    n $ run (cmd "cat")
