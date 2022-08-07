#!/usr/bin/env bake

import Bakery

main = bake do
  recipe (file "one.file") $ \output ->
    nullStdIn |> run "echo" "one" |> writeF output

  recipe (file "two.file") $ \output ->
    nullStdIn |> run "echo" "two" |> writeF output
