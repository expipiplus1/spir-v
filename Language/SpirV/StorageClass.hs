{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.StorageClass where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data StorageClass = UniformConstant 
                  | Input 
                  | Uniform 
                  | Output 
                  | WorkgroupLocal 
                  | WorkgroupGlobal 
                  | PrivateGlobal 
                  | Function 
                  | Generic 
                  | AtomicCounter 
                  | Image
  deriving(Read, Show, Eq, Ord)

instance SpirEnum StorageClass Word32 where
  toWord UniformConstant = 0
  toWord Input = 1
  toWord Uniform = 2
  toWord Output = 3
  toWord WorkgroupLocal = 4
  toWord WorkgroupGlobal = 5
  toWord PrivateGlobal = 6
  toWord Function = 7
  toWord Generic = 8
  toWord AtomicCounter = 10
  toWord Image = 11

  fromWord 0 = Just UniformConstant
  fromWord 1 = Just Input
  fromWord 2 = Just Uniform
  fromWord 3 = Just Output
  fromWord 4 = Just WorkgroupLocal
  fromWord 5 = Just WorkgroupGlobal
  fromWord 6 = Just PrivateGlobal
  fromWord 7 = Just Function
  fromWord 8 = Just Generic
  fromWord 10 = Just AtomicCounter
  fromWord 11 = Just Image
  fromWord _ = Nothing

  requiredCapabilities Input = [Capability.Shader]
  requiredCapabilities Uniform = [Capability.Shader]
  requiredCapabilities Output = [Capability.Shader]
  requiredCapabilities PrivateGlobal = [Capability.Shader]
  requiredCapabilities Generic = [Capability.Kernel]
  requiredCapabilities AtomicCounter = [Capability.AtomicStorage]
  requiredCapabilities _ = []
