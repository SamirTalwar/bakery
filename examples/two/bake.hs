import Bakery

main = bake do
  recipe (file "one.file") $ shell \output ->
    nullStdIn |> run "echo" "one" |> write output

  recipe (file "two.file") $ shell \output ->
    nullStdIn |> run "echo" "two" |> write output
