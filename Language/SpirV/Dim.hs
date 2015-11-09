{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.Dim where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data Dim = OneD 
         | TwoD 
         | ThreeD 
         | Cube 
         | Rect 
         | Buffer
  deriving(Read, Show, Eq, Ord)

instance SpirEnum Dim Word32 where
  toWord OneD = 0
  toWord TwoD = 1
  toWord ThreeD = 2
  toWord Cube = 3
  toWord Rect = 4
  toWord Buffer = 5

  fromWord 0 = Just OneD
  fromWord 1 = Just TwoD
  fromWord 2 = Just ThreeD
  fromWord 3 = Just Cube
  fromWord 4 = Just Rect
  fromWord 5 = Just Buffer
  fromWord _ = Nothing

  requiredCapabilities Cube = [Capability.Shader]
  requiredCapabilities Rect = [Capability.Shader]
  requiredCapabilities _ = []
