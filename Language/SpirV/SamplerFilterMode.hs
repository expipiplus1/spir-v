{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.SamplerFilterMode where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data SamplerFilterMode = Nearest 
                       | Linear
  deriving(Read, Show, Eq, Ord)

instance SpirEnum SamplerFilterMode Word32 where
  toWord Nearest = 0
  toWord Linear = 1

  fromWord 0 = Just Nearest
  fromWord 1 = Just Linear
  fromWord _ = Nothing

  requiredCapabilities Nearest = [Capability.Kernel]
  requiredCapabilities Linear = [Capability.Kernel]
  
