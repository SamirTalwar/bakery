import Bakery

main = bake do
  recipe (file "one.file") $ shell \output ->
    nullStdIn |> run "echo" "one" |> writeF output

  recipe (file "two.file") $ shell \output ->
    nullStdIn |> run "echo" "two" |> writeF output
