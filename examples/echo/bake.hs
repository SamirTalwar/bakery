import Bakery

main = bake do
  recipe (file "output.file") $ \output ->
    nullStdIn |> run "echo" "Hello," "world!" |> writeF output
