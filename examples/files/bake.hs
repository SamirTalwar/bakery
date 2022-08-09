#!/usr/bin/env bake

import Bakery

main = bake do
  existing <- existing $ file "existing.file"

  one <- recipe (file "one.file") $ \output ->
    nullStdIn |> run "echo" "one" |> writeF output

  two <- recipe (file "two.file") $ \output ->
    nullStdIn |> run "echo" "two" |> writeF output

  recipe (file "cat-existing.file") $ \output ->
    nullStdIn |> run "cat" existing one two |> writeF output

  recipe (file "cat.file") $ \output ->
    nullStdIn |> run "cat" one two |> writeF output
