#!/usr/bin/env bake

import Bakery

main = bake do
  hello <- recipe (file "hello.file") $ \output ->
    run "echo" "Hello," "world!" |> writeF output

  recipe (file "goodbye.file") $ \output ->
    run "echo" "Goodbye," "cruel" "world!" |> writeF output

  defaultRecipe hello
