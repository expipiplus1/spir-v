{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.SelectionControl where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data SelectionControl = None 
                      | Flatten 
                      | DontFlatten
  deriving(Read, Show, Eq, Ord)

instance SpirEnum SelectionControl Word32 where
  toWord None = 0
  toWord Flatten = 1
  toWord DontFlatten = 2

  fromWord 0 = Just None
  fromWord 1 = Just Flatten
  fromWord 2 = Just DontFlatten
  fromWord _ = Nothing

  
  requiredCapabilities _ = []
