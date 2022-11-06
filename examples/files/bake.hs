#!/usr/bin/env bake

import Bakery

main = bake do
  thisDirectory <- currentDirectory

  existing <- existing $ file "existing.file"

  one <- recipe (file "one.file") $ \output ->
    run_ "echo" "one" |> writeF output

  two <- recipe (file "two.file") $ \output ->
    run_ "echo" "two" |> writeF output

  three <- recipe (file "three.file") $ \output ->
    run_ "echo" "three" |> writeF output

  recipe (file "cat-existing.file") $ \output ->
    run_ "cat" existing one two |> writeF output

  cat <- recipe (file "cat.file") $ \output ->
    run_ "cat" one two |> writeF output

  recipe (file "cat-more.file") $ \output ->
    run_ "cat" cat three |> writeF output

  recipe (file "copy.file") $ \output ->
    run_ "cp" existing output

  recipe (file "two-phase.file") $ \output -> do
    let firstPhase = thisDirectory <> "/../tmp/first-phase.file"
    n $ run (cmd "echo" ~ "one" ~ "two") |> writeF firstPhase
    n $ run (cmd "cp" ~ firstPhase ~ output)

  recipe (file "non-existent.file") $ \output ->
    run_ "cat" one

  recipe (file "../files/./indirect.file") $ \output ->
    run_ "echo" "this got there eventually" |> writeF output

  recipe (file $ thisDirectory <> "/../tmp/output.file") $ \output ->
    run_ "echo" "This is a test." |> writeF output

  recipe (file $ thisDirectory <> "/../tmp/non-existent.file") $ \output ->
    run_ "cat"
