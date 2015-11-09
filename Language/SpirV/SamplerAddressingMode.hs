{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.SamplerAddressingMode where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data SamplerAddressingMode = None 
                           | ClampToEdge 
                           | Clamp 
                           | Repeat 
                           | RepeatMirrored
  deriving(Read, Show, Eq, Ord)

instance SpirEnum SamplerAddressingMode Word32 where
  toWord None = 0
  toWord ClampToEdge = 1
  toWord Clamp = 2
  toWord Repeat = 3
  toWord RepeatMirrored = 4

  fromWord 0 = Just None
  fromWord 1 = Just ClampToEdge
  fromWord 2 = Just Clamp
  fromWord 3 = Just Repeat
  fromWord 4 = Just RepeatMirrored
  fromWord _ = Nothing

  requiredCapabilities None = [Capability.Kernel]
  requiredCapabilities ClampToEdge = [Capability.Kernel]
  requiredCapabilities Clamp = [Capability.Kernel]
  requiredCapabilities Repeat = [Capability.Kernel]
  requiredCapabilities RepeatMirrored = [Capability.Kernel]
  
