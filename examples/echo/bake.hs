#!/usr/bin/env bake

import Bakery

main = bake do
  hello <- recipe (file "hello.file") $ \output ->
    run_ "echo" "Hello," "world!" |> writeF output

  recipe (file "goodbye.file") $ \output ->
    run_ "echo" "Goodbye," "cruel" "world!" |> writeF output

  defaultRecipe hello
