{-# LANGUAGE FunctionalDependencies #-}

module Language.SpirV.SpirEnum where

import {-# SOURCE #-} Language.SpirV.Capability

class SpirEnum a b | a -> b where
  toWord :: a -> b
  fromWord :: b -> Maybe a
  requiredCapabilities :: a -> [Capability]
