{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.GroupOperation where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data GroupOperation = Reduce 
                    | InclusiveScan 
                    | ExclusiveScan
  deriving(Read, Show, Eq, Ord)

instance SpirEnum GroupOperation Word32 where
  toWord Reduce = 0
  toWord InclusiveScan = 1
  toWord ExclusiveScan = 2

  fromWord 0 = Just Reduce
  fromWord 1 = Just InclusiveScan
  fromWord 2 = Just ExclusiveScan
  fromWord _ = Nothing

  requiredCapabilities Reduce = [Capability.Kernel]
  requiredCapabilities InclusiveScan = [Capability.Kernel]
  requiredCapabilities ExclusiveScan = [Capability.Kernel]
  
