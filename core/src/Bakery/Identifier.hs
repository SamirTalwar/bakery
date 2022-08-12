module Bakery.Identifier
  ( Identifiable (..),
    Id, -- the constructor and fields are intentionally not exported
    Namespace (..),
    Name (..),
  )
where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text

data Id = Id {idNamespace :: Namespace, idName :: Name}
  deriving stock (Eq)

instance Show Id where
  show Id {idNamespace, idName} = show idNamespace <> ":" <> show idName

newtype Namespace = Namespace {unNamespace :: Text}
  deriving newtype (Eq)

instance Show Namespace where
  show = Text.unpack . unNamespace

newtype Name = Name {unName :: Text}
  deriving newtype (Eq)

instance Show Name where
  show = Text.unpack . unName

class Identifiable a where
  namespace :: Proxy a -> Namespace
  name :: a -> Name

  identifier :: a -> Id
  identifier x = Id {idNamespace = namespace (Proxy :: Proxy a), idName = name x}
