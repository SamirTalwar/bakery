#!/usr/bin/env bake

import Bakery

main = bake do
  hello <- recipe (file "hello.file") $ shell do
    n $ run (cmd "echo" ~ "Hello," ~ "world!") |> writeF target

  recipe (file "goodbye.file") $ shell do
    n $ run (cmd "echo" ~ "Goodbye," ~ "cruel" ~ "world!") |> writeF target

  defaultRecipe hello
