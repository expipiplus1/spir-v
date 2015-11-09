{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.SourceLanguage where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data SourceLanguage = Unknown 
                    | ESSL 
                    | GLSL 
                    | OpenCL
  deriving(Read, Show, Eq, Ord)

instance SpirEnum SourceLanguage Word32 where
  toWord Unknown = 0
  toWord ESSL = 1
  toWord GLSL = 2
  toWord OpenCL = 3

  fromWord 0 = Just Unknown
  fromWord 1 = Just ESSL
  fromWord 2 = Just GLSL
  fromWord 3 = Just OpenCL
  fromWord _ = Nothing

  
  requiredCapabilities _ = []
