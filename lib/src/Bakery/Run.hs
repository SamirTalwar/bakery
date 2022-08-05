{-# LANGUAGE UndecidableInstances #-}

module Bakery.Run
  ( -- * Construction
    nullStdIn,
    nullStdOut,
    readF,
    writeF,
    run,
    (|>),

    -- * Evaluation
    type (#>),
    deriveShellInputs,
    evaluateShell,
  )
where

import Bakery.Bakeable (InShell (..), Input (..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import GHC.IO.Handle (hClose)
import System.Process.Typed qualified as Process

infixr 7 #>

data i #> o where
  NullStdIn :: () #> StdIn
  NullStdOut :: StdOut #> ()
  Run :: NonEmpty Arg -> StdIn #> StdOut
  Read :: Path -> () #> StdIn
  Write :: Path -> StdOut #> ()
  Compose :: a #> b -> b #> c -> a #> c

deriving stock instance Show (i #> o)

data StdIn = StdIn Text

data StdOut = StdOut Text

data Path where
  InputPath :: forall a. InShell a => a -> Path

deriving stock instance Show Path

nullStdIn :: () #> StdIn
nullStdIn = NullStdIn

nullStdOut :: StdOut #> ()
nullStdOut = NullStdOut

readF :: InShell a => a -> () #> StdIn
readF = Read . InputPath

writeF :: InShell a => a -> StdOut #> ()
writeF = Write . InputPath

infixr 5 |>

(|>) :: a #> b -> b #> c -> a #> c
(|>) = Compose

data Arg where
  StringArg :: String -> Arg
  InputArg :: InShell a => a -> Arg

deriving stock instance Show Arg

class Argument a where
  toArg :: a -> Arg

instance {-# OVERLAPPING #-} Argument String where
  toArg = StringArg

instance {-# OVERLAPPABLE #-} InShell a => Argument a where
  toArg = InputArg

fromArg :: Arg -> String
fromArg (StringArg arg) = arg
fromArg (InputArg arg) = inShell arg

class RunOutput r where
  run' :: NonEmpty Arg -> r

instance (Argument a, RunOutput r) => RunOutput (a -> r) where
  run' args arg = run' (toArg arg `NonEmpty.cons` args)

instance RunOutput (StdIn #> StdOut) where
  run' args = Run (NonEmpty.reverse args)

run :: (Argument a, RunOutput r) => a -> r
run arg = run' (NonEmpty.singleton (toArg arg))

deriveShellInputs :: i #> o -> [Input]
deriveShellInputs NullStdIn = []
deriveShellInputs NullStdOut = []
deriveShellInputs (Run args) =
  toList args >>= \case
    StringArg _ -> []
    InputArg arg -> [Input arg]
deriveShellInputs (Read (InputPath input)) = [Input input]
deriveShellInputs (Write (InputPath input)) = [Input input]
deriveShellInputs (Compose a b) = deriveShellInputs a <> deriveShellInputs b

evaluateShell :: () #> () -> IO ()
evaluateShell program = evaluateShell' program ()

evaluateShell' :: a #> b -> a -> IO b
evaluateShell' NullStdIn () =
  pure $ StdIn Text.empty
evaluateShell' NullStdOut (StdOut _) =
  pure ()
evaluateShell' (Run (cmd :| args)) (StdIn stdin) =
  let config =
        Process.setStdin Process.createPipe $
          Process.setStdout Process.createPipe $
            Process.proc (fromArg cmd) (map fromArg args)
   in StdOut <$> Process.withProcessWait config \process -> do
        let hStdin = Process.getStdin process
            hStdout = Process.getStdout process
        Text.IO.hPutStr hStdin stdin
        hClose hStdin
        Text.IO.hGetContents hStdout
evaluateShell' (Read (InputPath input)) () =
  StdIn <$> Text.IO.readFile (inShell input)
evaluateShell' (Write (InputPath input)) (StdOut text) =
  Text.IO.writeFile (inShell input) text
evaluateShell' (Compose a b) i =
  evaluateShell' a i >>= evaluateShell' b
