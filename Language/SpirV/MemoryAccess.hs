{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.MemoryAccess where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data MemoryAccess = None 
                  | Volatile 
                  | Aligned
  deriving(Read, Show, Eq, Ord)

instance SpirEnum MemoryAccess Word32 where
  toWord None = 0
  toWord Volatile = 1
  toWord Aligned = 2

  fromWord 0 = Just None
  fromWord 1 = Just Volatile
  fromWord 2 = Just Aligned
  fromWord _ = Nothing

  
  requiredCapabilities _ = []
