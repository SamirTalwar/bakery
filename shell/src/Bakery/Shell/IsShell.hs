{-# LANGUAGE FunctionalDependencies #-}

module Bakery.Shell.IsShell (IsShell (..)) where

import Bakery.Shell.Shell (Shell)

class IsShell m i o a | a -> m, a -> i, a -> o where
  shell :: Shell m i o () -> a
