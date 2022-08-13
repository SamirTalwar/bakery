#!/usr/bin/env bake

-- This is fiction. It does not work.
-- For now, we use `test.sh` to build and test the Bakery.
-- Eventually, this should supersede it, but we're not there yet.

import Bakery

packages = ["core", "lib", "shell", "examples"]

main = bake do
  root <- currentDirectory
  binPath <- recipe env do
    Env.modify "PATH" (\path -> root <> "/bin:" <> path)

  libraries <- each packages $ \package ->
    inDirectory package do
      src <- existing $ glob "src/**"

      packageFile <- existing $ file "package.yaml"
      packageDescription <- parse packageFile
      let name = packageDescription .: "name"
      cabalFile <- recipe (file (name <> ".cabal")) $ const do
        input src
        run "hpack" packageFile

      recipe (exec "build") do
        input cabalFile
        run "cabal" "build"

  examples <- glob ("examples/*") {only = Glob.directories}
  each_ examples $ \example ->
    inDirectory example do
      let cabalName = "example-" <> basename example
      recipe (exec "build") do
        run "cabal" "build" cabalName
      recipe (exec "run") do
        run "cabal" "run" cabalName

      recipe (exec "smoke") do
        input build
        with binPath do
          run "cabal" "exec" "--" "smoke" "."

  smoke <- do
    smokeFiles <- existing $ glob "examples/*/smoke.yaml"
    recipe (exec "smoke") do
      input libraries
      with binPath do
        run "cabal" "exec" "--" "smoke" smokeFiles

  srcFiles <- existing $ glob "src/**"
  hlint <- recipe (exec "hlint") do
    run "hlint" srcFiles
  ormolu <- recipe (exec "ormolu") do
    run "ormolu" "--mode=check" srcFiles

  recipe (group "check") do
    smoke
    hlint
    ormolu
