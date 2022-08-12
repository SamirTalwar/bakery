{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Bakery.A where

import Data.Kind (Type)

data A (f :: Type -> Type) where
  A :: forall f a. f a -> A f

instance forall f. (forall a. Show (f a)) => Show (A f) where
  show (A x) = show x

type An = A

pattern An :: f a -> A f
pattern An x = A x

{-# COMPLETE An :: A #-}
