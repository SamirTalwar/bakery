{-# LANGUAGE FunctionalDependencies #-}

module Bakery.Shell.IsShell (IsShell (..)) where

import Bakery.Shell.Shell (Shell)

class IsShell m i o a | a -> m where
  shell :: Shell m i o () -> a
