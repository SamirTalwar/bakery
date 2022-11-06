#!/usr/bin/env bake

import Bakery

main = bake do
  hello <- recipe (file "hello.file") do
    n $ run (cmd "echo" ~ "Hello," ~ "world!") |> writeF target

  recipe (file "goodbye.file") do
    n $ run (cmd "echo" ~ "Goodbye," ~ "cruel" ~ "world!") |> writeF target

  defaultRecipe hello
