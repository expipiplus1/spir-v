{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.KernelEnqueueFlags where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data KernelEnqueueFlags = NoWait 
                        | WaitKernel 
                        | WaitWorkGroup
  deriving(Read, Show, Eq, Ord)

instance SpirEnum KernelEnqueueFlags Word32 where
  toWord NoWait = 0
  toWord WaitKernel = 1
  toWord WaitWorkGroup = 2

  fromWord 0 = Just NoWait
  fromWord 1 = Just WaitKernel
  fromWord 2 = Just WaitWorkGroup
  fromWord _ = Nothing

  requiredCapabilities NoWait = [Capability.Kernel]
  requiredCapabilities WaitKernel = [Capability.Kernel]
  requiredCapabilities WaitWorkGroup = [Capability.Kernel]
  
