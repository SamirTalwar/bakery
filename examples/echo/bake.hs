#!/usr/bin/env bake

import Bakery

main = bake do
  hello <- recipe (file "hello.file") $ \output ->
    n $ run (cmd "echo" "Hello," "world!") |> writeF output

  recipe (file "goodbye.file") $ \output ->
    n $ run (cmd "echo" "Goodbye," "cruel" "world!") |> writeF output

  defaultRecipe hello
