{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.FPRoundingMode where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data FPRoundingMode = RTE 
                    | RTZ 
                    | RTP 
                    | RTN
  deriving(Read, Show, Eq, Ord)

instance SpirEnum FPRoundingMode Word32 where
  toWord RTE = 0
  toWord RTZ = 1
  toWord RTP = 2
  toWord RTN = 3

  fromWord 0 = Just RTE
  fromWord 1 = Just RTZ
  fromWord 2 = Just RTP
  fromWord 3 = Just RTN
  fromWord _ = Nothing

  requiredCapabilities RTE = [Capability.Kernel]
  requiredCapabilities RTZ = [Capability.Kernel]
  requiredCapabilities RTP = [Capability.Kernel]
  requiredCapabilities RTN = [Capability.Kernel]
  
