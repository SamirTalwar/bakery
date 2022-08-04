import Bakery

main = bake do
  recipe (file "output.file") $ shell \output ->
    nullStdIn |> run "echo" "Hello," "world!" |> writeF output
