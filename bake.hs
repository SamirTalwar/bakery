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
      cabalFile <- recipe (file "bakery-core.cabal") $ const do
        input src
        run "hpack" packageFile

      named "build" $ recipe exec do
        input cabalFile
        run "cabal" "build"

  examples <- glob ("examples/*") {only = Glob.directories}
  each_ examples $ \example ->
    inDirectory example do
      let cabalName = "example-" <> basename example
      named "build" $ recipe exec do
        run "cabal" "build" cabalName
      named "run" $ recipe exec do
        run "cabal" "run" cabalName

      named "smoke" $ recipe exec do
        input build
        with binPath do
          run "cabal" "exec" "--" "smoke" "."

  smoke <- do
    smokeFiles <- existing $ glob "examples/*/smoke.yaml"
    named "smoke" $ recipe exec do
      input libraries
      with binPath do
        run "cabal" "exec" "--" "smoke" smokeFiles

  srcFiles <- existing $ glob "src/**"
  hlint <- named "hlint" $ recipe exec do
    run "hlint" srcFiles
  ormolu <- named "ormolu" $ recipe ormolu do
    run "ormolu" "--mode=check" srcFiles

  named "check" $ recipe group do
    smoke
    hlint
    ormolu
