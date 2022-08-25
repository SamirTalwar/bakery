#!/usr/bin/env bake

import Bakery

main = bake do
  thisDirectory <- currentDirectory

  existing <- existing $ file "existing.file"

  one <- recipe (file "one.file") $ \output ->
    run "echo" "one" |> writeF output

  two <- recipe (file "two.file") $ \output ->
    run "echo" "two" |> writeF output

  recipe (file "cat-existing.file") $ \output ->
    run "cat" existing one two |> writeF output

  recipe (file "cat.file") $ \output ->
    run "cat" one two |> writeF output

  recipe (file "copy.file") $ \output ->
    run "cp" existing output

  recipe (file "non-existent.file") $ \output ->
    run "cat" one

  recipe (file "../files/./indirect.file") $ \output ->
    run "echo" "this got there eventually" |> writeF output

  recipe (file $ thisDirectory <> "/../tmp/output.file") $ \output ->
    run "echo" "This is a test." |> writeF output

  recipe (file $ thisDirectory <> "/../tmp/non-existent.file") $ \output ->
    run "cat"
