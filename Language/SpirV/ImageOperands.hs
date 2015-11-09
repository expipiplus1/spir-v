{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.ImageOperands where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data ImageOperands = None 
                   | Bias 
                   | Lod 
                   | Grad 
                   | ConstOffset 
                   | Offset 
                   | ConstOffsets 
                   | Sample
  deriving(Read, Show, Eq, Ord)

instance SpirEnum ImageOperands Word32 where
  toWord None = 0
  toWord Bias = 1
  toWord Lod = 2
  toWord Grad = 4
  toWord ConstOffset = 8
  toWord Offset = 16
  toWord ConstOffsets = 32
  toWord Sample = 64

  fromWord 0 = Just None
  fromWord 1 = Just Bias
  fromWord 2 = Just Lod
  fromWord 4 = Just Grad
  fromWord 8 = Just ConstOffset
  fromWord 16 = Just Offset
  fromWord 32 = Just ConstOffsets
  fromWord 64 = Just Sample
  fromWord _ = Nothing

  requiredCapabilities Bias = [Capability.Shader]
  requiredCapabilities Offset = [Capability.ImageGatherExtended]
  requiredCapabilities _ = []
