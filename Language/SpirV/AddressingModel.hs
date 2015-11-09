{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.AddressingModel where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data AddressingModel = Logical 
                     | Physical32 
                     | Physical64
  deriving(Read, Show, Eq, Ord)

instance SpirEnum AddressingModel Word32 where
  toWord Logical = 0
  toWord Physical32 = 1
  toWord Physical64 = 2

  fromWord 0 = Just Logical
  fromWord 1 = Just Physical32
  fromWord 2 = Just Physical64
  fromWord _ = Nothing

  requiredCapabilities Physical32 = [Capability.Addresses]
  requiredCapabilities Physical64 = [Capability.Addresses]
  requiredCapabilities _ = []
