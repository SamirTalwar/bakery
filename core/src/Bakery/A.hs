{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Bakery.A where

import Data.Kind (Constraint, Type)

data A (f :: Type -> Type) where
  A :: f a -> A f

instance (forall a. Show (f a)) => Show (A f) where
  show (A x) = show x

type An = A

pattern An :: f a -> A f
pattern An x = A x

{-# COMPLETE An :: A #-}

data Is (c :: Type -> Constraint) where
  Is :: forall a c. c a => Is c
