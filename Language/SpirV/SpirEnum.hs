{-# LANGUAGE FunctionalDependencies #-}

module Language.SpirV.SpirEnum where

import {-# SOURCE #-} Language.SpirV.Capability

class SpirEnum a word | a -> word where
  toWord :: a -> word
  fromWord :: word -> Maybe a
  requiredCapabilities :: a -> [Capability]
