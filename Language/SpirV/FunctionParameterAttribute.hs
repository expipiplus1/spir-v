{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.FunctionParameterAttribute where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data FunctionParameterAttribute = Zext 
                                | Sext 
                                | ByVal 
                                | Sret 
                                | NoAlias 
                                | NoCapture 
                                | NoWrite 
                                | NoReadWrite
  deriving(Read, Show, Eq, Ord)

instance SpirEnum FunctionParameterAttribute Word32 where
  toWord Zext = 0
  toWord Sext = 1
  toWord ByVal = 2
  toWord Sret = 3
  toWord NoAlias = 4
  toWord NoCapture = 5
  toWord NoWrite = 6
  toWord NoReadWrite = 7

  fromWord 0 = Just Zext
  fromWord 1 = Just Sext
  fromWord 2 = Just ByVal
  fromWord 3 = Just Sret
  fromWord 4 = Just NoAlias
  fromWord 5 = Just NoCapture
  fromWord 6 = Just NoWrite
  fromWord 7 = Just NoReadWrite
  fromWord _ = Nothing

  requiredCapabilities Zext = [Capability.Kernel]
  requiredCapabilities Sext = [Capability.Kernel]
  requiredCapabilities ByVal = [Capability.Kernel]
  requiredCapabilities Sret = [Capability.Kernel]
  requiredCapabilities NoAlias = [Capability.Kernel]
  requiredCapabilities NoCapture = [Capability.Kernel]
  requiredCapabilities NoWrite = [Capability.Kernel]
  requiredCapabilities NoReadWrite = [Capability.Kernel]
  
