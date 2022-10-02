{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Streams are lazy, effectful sequences.
--
-- The concept is taken from the paper,
-- "Representations of Stream Processors Using Nested Fixed Points".
-- Link: https://arxiv.org/abs/0905.4813
module Bakery.Shell.Stream
  ( Stream (..),

    -- * Constructors
    stop,
    (#:),
    demand,

    -- * Converters
    fromList,
    toListM,
  )
where

import Data.Functor ((<&>))
import Data.Kind (Type)

-- | 'Stream' models a lazy, effectful sequence.
type Stream :: Type -> (Type -> Type) -> Type -> Type
newtype Stream i m o = Stream {runStream :: m (Stream' i m o)}

data Stream' (i :: Type) (m :: Type -> Type) (o :: Type) where
  Stop :: Stream' i m o
  (:#) :: o -> Stream i m o -> Stream' i m o
  Demand :: (i -> Stream i m o) -> Stream' i m o

stream :: Applicative m => Stream' i m o -> Stream i m o
stream = Stream . pure

-- | An empty stream.
stop :: Applicative m => Stream i m o
stop = stream Stop

-- | Prepends a value to an existing stream.
infixr 7 #:

(#:) :: Applicative m => o -> Stream i m o -> Stream i m o
value #: next = stream $ value :# next

-- | Demands a value in order to produce a stream.
demand :: Applicative m => (i -> Stream i m o) -> Stream i m o
demand onNext = stream $ Demand onNext

instance Monad m => Semigroup (Stream' i m o) where
  Stop <> y' = y'
  value :# Stream next <> y = value :# Stream (next <&> (<> y))
  Demand onNext <> y = Demand \value -> Stream (runStream (onNext value) <&> (<> y))

instance Monad m => Semigroup (Stream i m o) where
  Stream x <> Stream y = Stream $ (<>) <$> x <*> y

instance Monad m => Monoid (Stream' i m o) where
  mempty = Stop

instance Monad m => Monoid (Stream i m o) where
  mempty = stream mempty

-- | Converts a list to a producer stream.
fromList :: Monad m => [o] -> Stream () m o
fromList = foldr (#:) stop

-- | Effectfully converts a producer stream to a list.
toListM :: Monad m => Stream () m o -> m [o]
toListM (Stream s) =
  s >>= \case
    Stop -> pure []
    value :# next -> (value :) <$> toListM next
    Demand onNext -> toListM $ onNext ()
