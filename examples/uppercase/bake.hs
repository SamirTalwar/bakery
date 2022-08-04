import Bakery

main = bake do
  input <- existing $ file "input.file"

  recipe (file "output.file") $ shell \output ->
    readF input |> run "tr" "[:lower:]" "[:upper:]" |> writeF output
