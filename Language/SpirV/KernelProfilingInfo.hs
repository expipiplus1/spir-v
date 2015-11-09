{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.KernelProfilingInfo where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data KernelProfilingInfo = None 
                         | CmdExecTime
  deriving(Read, Show, Eq, Ord)

instance SpirEnum KernelProfilingInfo Word32 where
  toWord None = 0
  toWord CmdExecTime = 1

  fromWord 0 = Just None
  fromWord 1 = Just CmdExecTime
  fromWord _ = Nothing

  requiredCapabilities CmdExecTime = [Capability.Kernel]
  requiredCapabilities _ = []
