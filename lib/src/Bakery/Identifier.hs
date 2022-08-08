module Bakery.Identifier
  ( Id (..),
    Identifiable (..),
  )
where

data Id = Id {idType :: String, idTarget :: String}
  deriving stock (Eq)

instance Show Id where
  show Id {idType, idTarget} = idType <> ":" <> idTarget

class Identifiable a where
  identifier :: a -> Id
