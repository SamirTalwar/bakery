module Bakery.Bakeable where

class Bakeable a where
  data Recipe a
  follow :: Recipe a -> IO a
