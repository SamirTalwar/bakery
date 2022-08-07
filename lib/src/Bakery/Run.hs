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
    StdIn (..),
    StdOut (..),
    Path (..),
    InputPath (..),
    OutputPath (..),
    deriveShellInputs,
    evaluateShell,
  )
where

import Bakery.Bakeable (Inputs)
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
  Read :: InputPath -> () #> StdIn
  Write :: OutputPath -> StdOut #> ()
  Compose :: a #> b -> b #> c -> a #> c

deriving stock instance Show (i #> o)

newtype StdIn where
  StdIn :: Text -> StdIn

newtype StdOut where
  StdOut :: Text -> StdOut

data InputPath where
  InputPath :: Inputs -> FilePath -> InputPath

deriving stock instance Show InputPath

data OutputPath where
  KnownOutputPath :: FilePath -> OutputPath
  UnknownOutputPath :: OutputPath

deriving stock instance Show OutputPath

class Path a where
  toInputPath :: a -> InputPath

nullStdIn :: () #> StdIn
nullStdIn = NullStdIn

nullStdOut :: StdOut #> ()
nullStdOut = NullStdOut

readF :: Path a => a -> () #> StdIn
readF = Read . toInputPath

writeF :: OutputPath -> StdOut #> ()
writeF = Write

infixr 5 |>

(|>) :: a #> b -> b #> c -> a #> c
(|>) = Compose

data Arg where
  StringArg :: String -> Arg
  IntegerArg :: Integer -> Arg
  PathArg :: InputPath -> Arg

deriving stock instance Show Arg

class Argument a where
  toArg :: a -> Arg

instance Argument String where
  toArg = StringArg

instance Argument Integer where
  toArg = IntegerArg

instance Argument InputPath where
  toArg = PathArg

fromArg :: Arg -> String
fromArg (StringArg arg) = arg
fromArg (IntegerArg arg) = show arg
fromArg (PathArg (InputPath _ arg)) = arg

run :: (Argument a, RunType r) => a -> r
run arg = run' (NonEmpty.singleton (toArg arg))

class RunType r where
  run' :: NonEmpty Arg -> r

instance (Argument a, RunType r) => RunType (a -> r) where
  run' args arg = run' (toArg arg `NonEmpty.cons` args)

instance RunType (StdIn #> StdOut) where
  run' args = Run (NonEmpty.reverse args)

deriveShellInputs :: i #> o -> Inputs
deriveShellInputs NullStdIn = []
deriveShellInputs NullStdOut = []
deriveShellInputs (Run args) =
  toList args >>= \case
    StringArg _ -> []
    IntegerArg _ -> []
    PathArg (InputPath inputs _) -> inputs
deriveShellInputs (Read (InputPath inputs _)) = inputs
deriveShellInputs (Write _) = []
deriveShellInputs (Compose a b) = deriveShellInputs a <> deriveShellInputs b

-- The argument order is important, so that 'a' is constrained by 'a #> b'.
evaluateShell :: a #> b -> a -> IO b
evaluateShell NullStdIn () =
  pure $ StdIn Text.empty
evaluateShell NullStdOut (StdOut _) =
  pure ()
evaluateShell (Run (cmd :| args)) (StdIn stdin) =
  let config =
        Process.setStdin Process.createPipe $
          Process.setStdout Process.createPipe $
            Process.proc (fromArg cmd) (map fromArg args)
   in StdOut <$> Process.withProcessWait_ config \process -> do
        let hStdin = Process.getStdin process
            hStdout = Process.getStdout process
        Text.IO.hPutStr hStdin stdin
        hClose hStdin
        Text.IO.hGetContents hStdout
evaluateShell (Read (InputPath _ path)) () =
  StdIn <$> Text.IO.readFile path
evaluateShell (Write (KnownOutputPath path)) (StdOut text) =
  Text.IO.writeFile path text
evaluateShell (Write UnknownOutputPath) (StdOut _) =
  fail "INTERNAL ERROR: Cannot write to an unknown path."
evaluateShell (Compose a b) i =
  evaluateShell a i >>= evaluateShell b
