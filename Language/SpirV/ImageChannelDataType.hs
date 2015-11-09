{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.ImageChannelDataType where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data ImageChannelDataType = SnormInt8 
                          | SnormInt16 
                          | UnormInt8 
                          | UnormInt16 
                          | UnormShort565 
                          | UnormShort555 
                          | UnormInt101010 
                          | SignedInt8 
                          | SignedInt16 
                          | SignedInt32 
                          | UnsignedInt8 
                          | UnsignedInt16 
                          | UnsignedInt32 
                          | HalfFloat 
                          | Float 
                          | UnormInt24
  deriving(Read, Show, Eq, Ord)

instance SpirEnum ImageChannelDataType Word32 where
  toWord SnormInt8 = 0
  toWord SnormInt16 = 1
  toWord UnormInt8 = 2
  toWord UnormInt16 = 3
  toWord UnormShort565 = 4
  toWord UnormShort555 = 5
  toWord UnormInt101010 = 6
  toWord SignedInt8 = 7
  toWord SignedInt16 = 8
  toWord SignedInt32 = 9
  toWord UnsignedInt8 = 10
  toWord UnsignedInt16 = 11
  toWord UnsignedInt32 = 12
  toWord HalfFloat = 13
  toWord Float = 14
  toWord UnormInt24 = 15

  fromWord 0 = Just SnormInt8
  fromWord 1 = Just SnormInt16
  fromWord 2 = Just UnormInt8
  fromWord 3 = Just UnormInt16
  fromWord 4 = Just UnormShort565
  fromWord 5 = Just UnormShort555
  fromWord 6 = Just UnormInt101010
  fromWord 7 = Just SignedInt8
  fromWord 8 = Just SignedInt16
  fromWord 9 = Just SignedInt32
  fromWord 10 = Just UnsignedInt8
  fromWord 11 = Just UnsignedInt16
  fromWord 12 = Just UnsignedInt32
  fromWord 13 = Just HalfFloat
  fromWord 14 = Just Float
  fromWord 15 = Just UnormInt24
  fromWord _ = Nothing

  requiredCapabilities SnormInt8 = [Capability.Kernel]
  requiredCapabilities SnormInt16 = [Capability.Kernel]
  requiredCapabilities UnormInt8 = [Capability.Kernel]
  requiredCapabilities UnormInt16 = [Capability.Kernel]
  requiredCapabilities UnormShort565 = [Capability.Kernel]
  requiredCapabilities UnormShort555 = [Capability.Kernel]
  requiredCapabilities UnormInt101010 = [Capability.Kernel]
  requiredCapabilities SignedInt8 = [Capability.Kernel]
  requiredCapabilities SignedInt16 = [Capability.Kernel]
  requiredCapabilities SignedInt32 = [Capability.Kernel]
  requiredCapabilities UnsignedInt8 = [Capability.Kernel]
  requiredCapabilities UnsignedInt16 = [Capability.Kernel]
  requiredCapabilities UnsignedInt32 = [Capability.Kernel]
  requiredCapabilities HalfFloat = [Capability.Kernel]
  requiredCapabilities Float = [Capability.Kernel]
  requiredCapabilities UnormInt24 = [Capability.Kernel]
  
