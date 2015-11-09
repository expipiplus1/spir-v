{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.MemoryModel where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data MemoryModel = Simple 
                 | GLSL450 
                 | OpenCL
  deriving(Read, Show, Eq, Ord)

instance SpirEnum MemoryModel Word32 where
  toWord Simple = 0
  toWord GLSL450 = 1
  toWord OpenCL = 2

  fromWord 0 = Just Simple
  fromWord 1 = Just GLSL450
  fromWord 2 = Just OpenCL
  fromWord _ = Nothing

  requiredCapabilities Simple = [Capability.Shader]
  requiredCapabilities GLSL450 = [Capability.Shader]
  requiredCapabilities OpenCL = [Capability.Kernel]
  
