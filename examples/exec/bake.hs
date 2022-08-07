import Bakery

main = bake do
  recipe (exec "count") do
    run "seq" 1 10
