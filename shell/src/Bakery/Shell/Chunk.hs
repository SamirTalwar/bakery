{-# LANGUAGE DeriveFoldable #-}

module Bakery.Shell.Chunk
  ( Chunk (..),
    capped,
    consume,
  )
where

import Pipes
import Pipes.Prelude qualified as P

data Chunk a = Value a | End
  deriving stock (Eq, Foldable)

instance Show a => Show (Chunk a) where
  show (Value x) = show x
  show End = "<end>"

instance Semigroup a => Semigroup (Chunk a) where
  a <> End = a
  End <> b = b
  Value a <> Value b = Value (a <> b)

instance Semigroup a => Monoid (Chunk a) where
  mempty = End

capped :: Functor m => Pipe a b m () -> Pipe a (Chunk b) m ()
capped pipe = (pipe >-> P.map Value) <> yield End

consume :: Monad m => (a -> m ()) -> m () -> Pipe (Chunk a) b m ()
consume withValue onEnd =
  await >>= \case
    Value value -> lift (withValue value) >> consume withValue onEnd
    End -> lift onEnd
