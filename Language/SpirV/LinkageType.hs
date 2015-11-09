{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.LinkageType where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data LinkageType = Export 
                 | Import
  deriving(Read, Show, Eq, Ord)

instance SpirEnum LinkageType Word32 where
  toWord Export = 0
  toWord Import = 1

  fromWord 0 = Just Export
  fromWord 1 = Just Import
  fromWord _ = Nothing

  requiredCapabilities Export = [Capability.Linkage]
  requiredCapabilities Import = [Capability.Linkage]
  
