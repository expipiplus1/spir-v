{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.AccessQualifier where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data AccessQualifier = ReadOnly 
                     | WriteOnly 
                     | ReadWrite
  deriving(Read, Show, Eq, Ord)

instance SpirEnum AccessQualifier Word32 where
  toWord ReadOnly = 0
  toWord WriteOnly = 1
  toWord ReadWrite = 2

  fromWord 0 = Just ReadOnly
  fromWord 1 = Just WriteOnly
  fromWord 2 = Just ReadWrite
  fromWord _ = Nothing

  requiredCapabilities ReadOnly = [Capability.Kernel]
  requiredCapabilities WriteOnly = [Capability.Kernel]
  requiredCapabilities ReadWrite = [Capability.Kernel]
  
