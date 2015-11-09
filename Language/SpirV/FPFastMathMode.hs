{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.FPFastMathMode where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data FPFastMathMode = None 
                    | NotNaN 
                    | NotInf 
                    | NSZ 
                    | AllowRecip 
                    | Fast
  deriving(Read, Show, Eq, Ord)

instance SpirEnum FPFastMathMode Word32 where
  toWord None = 0
  toWord NotNaN = 1
  toWord NotInf = 2
  toWord NSZ = 4
  toWord AllowRecip = 8
  toWord Fast = 16

  fromWord 0 = Just None
  fromWord 1 = Just NotNaN
  fromWord 2 = Just NotInf
  fromWord 4 = Just NSZ
  fromWord 8 = Just AllowRecip
  fromWord 16 = Just Fast
  fromWord _ = Nothing

  requiredCapabilities NotNaN = [Capability.Kernel]
  requiredCapabilities NotInf = [Capability.Kernel]
  requiredCapabilities NSZ = [Capability.Kernel]
  requiredCapabilities AllowRecip = [Capability.Kernel]
  requiredCapabilities Fast = [Capability.Kernel]
  requiredCapabilities _ = []
