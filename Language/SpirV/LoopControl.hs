{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.LoopControl where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data LoopControl = None 
                 | Unroll 
                 | DontUnroll
  deriving(Read, Show, Eq, Ord)

instance SpirEnum LoopControl Word32 where
  toWord None = 0
  toWord Unroll = 1
  toWord DontUnroll = 2

  fromWord 0 = Just None
  fromWord 1 = Just Unroll
  fromWord 2 = Just DontUnroll
  fromWord _ = Nothing

  
  requiredCapabilities _ = []
