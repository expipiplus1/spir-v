{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.ImageFormat where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data ImageFormat = Unknown 
                 | Rgba32f 
                 | Rgba16f 
                 | R32f 
                 | Rgba8 
                 | Rgba8Snorm 
                 | Rg32f 
                 | Rg16f 
                 | R11fG11fB10f 
                 | R16f 
                 | Rgba16 
                 | Rgb10A2 
                 | Rg16 
                 | Rg8 
                 | R16 
                 | R8 
                 | Rgba16Snorm 
                 | Rg16Snorm 
                 | Rg8Snorm 
                 | R16Snorm 
                 | R8Snorm 
                 | Rgba32i 
                 | Rgba16i 
                 | Rgba8i 
                 | R32i 
                 | Rg32i 
                 | Rg16i 
                 | Rg8i 
                 | R16i 
                 | R8i 
                 | Rgba32ui 
                 | Rgba16ui 
                 | Rgba8ui 
                 | R32ui 
                 | Rgb10a2ui 
                 | Rg32ui 
                 | Rg16ui 
                 | Rg8ui 
                 | R16ui 
                 | R8ui
  deriving(Read, Show, Eq, Ord)

instance SpirEnum ImageFormat Word32 where
  toWord Unknown = 0
  toWord Rgba32f = 1
  toWord Rgba16f = 2
  toWord R32f = 3
  toWord Rgba8 = 4
  toWord Rgba8Snorm = 5
  toWord Rg32f = 6
  toWord Rg16f = 7
  toWord R11fG11fB10f = 8
  toWord R16f = 9
  toWord Rgba16 = 10
  toWord Rgb10A2 = 11
  toWord Rg16 = 12
  toWord Rg8 = 13
  toWord R16 = 14
  toWord R8 = 15
  toWord Rgba16Snorm = 16
  toWord Rg16Snorm = 17
  toWord Rg8Snorm = 18
  toWord R16Snorm = 19
  toWord R8Snorm = 20
  toWord Rgba32i = 21
  toWord Rgba16i = 22
  toWord Rgba8i = 23
  toWord R32i = 24
  toWord Rg32i = 25
  toWord Rg16i = 26
  toWord Rg8i = 27
  toWord R16i = 28
  toWord R8i = 29
  toWord Rgba32ui = 30
  toWord Rgba16ui = 31
  toWord Rgba8ui = 32
  toWord R32ui = 33
  toWord Rgb10a2ui = 34
  toWord Rg32ui = 35
  toWord Rg16ui = 36
  toWord Rg8ui = 37
  toWord R16ui = 38
  toWord R8ui = 39

  fromWord 0 = Just Unknown
  fromWord 1 = Just Rgba32f
  fromWord 2 = Just Rgba16f
  fromWord 3 = Just R32f
  fromWord 4 = Just Rgba8
  fromWord 5 = Just Rgba8Snorm
  fromWord 6 = Just Rg32f
  fromWord 7 = Just Rg16f
  fromWord 8 = Just R11fG11fB10f
  fromWord 9 = Just R16f
  fromWord 10 = Just Rgba16
  fromWord 11 = Just Rgb10A2
  fromWord 12 = Just Rg16
  fromWord 13 = Just Rg8
  fromWord 14 = Just R16
  fromWord 15 = Just R8
  fromWord 16 = Just Rgba16Snorm
  fromWord 17 = Just Rg16Snorm
  fromWord 18 = Just Rg8Snorm
  fromWord 19 = Just R16Snorm
  fromWord 20 = Just R8Snorm
  fromWord 21 = Just Rgba32i
  fromWord 22 = Just Rgba16i
  fromWord 23 = Just Rgba8i
  fromWord 24 = Just R32i
  fromWord 25 = Just Rg32i
  fromWord 26 = Just Rg16i
  fromWord 27 = Just Rg8i
  fromWord 28 = Just R16i
  fromWord 29 = Just R8i
  fromWord 30 = Just Rgba32ui
  fromWord 31 = Just Rgba16ui
  fromWord 32 = Just Rgba8ui
  fromWord 33 = Just R32ui
  fromWord 34 = Just Rgb10a2ui
  fromWord 35 = Just Rg32ui
  fromWord 36 = Just Rg16ui
  fromWord 37 = Just Rg8ui
  fromWord 38 = Just R16ui
  fromWord 39 = Just R8ui
  fromWord _ = Nothing

  requiredCapabilities Unknown = [Capability.Shader]
  requiredCapabilities Rgba32f = [Capability.Shader]
  requiredCapabilities Rgba16f = [Capability.Shader]
  requiredCapabilities R32f = [Capability.Shader]
  requiredCapabilities Rgba8 = [Capability.Shader]
  requiredCapabilities Rgba8Snorm = [Capability.Shader]
  requiredCapabilities Rg32f = [Capability.Shader]
  requiredCapabilities Rg16f = [Capability.Shader]
  requiredCapabilities R11fG11fB10f = [Capability.Shader]
  requiredCapabilities R16f = [Capability.Shader]
  requiredCapabilities Rgba16 = [Capability.Shader]
  requiredCapabilities Rgb10A2 = [Capability.Shader]
  requiredCapabilities Rg16 = [Capability.Shader]
  requiredCapabilities Rg8 = [Capability.Shader]
  requiredCapabilities R16 = [Capability.Shader]
  requiredCapabilities R8 = [Capability.Shader]
  requiredCapabilities Rgba16Snorm = [Capability.Shader]
  requiredCapabilities Rg16Snorm = [Capability.Shader]
  requiredCapabilities Rg8Snorm = [Capability.Shader]
  requiredCapabilities R16Snorm = [Capability.Shader]
  requiredCapabilities R8Snorm = [Capability.Shader]
  requiredCapabilities Rgba32i = [Capability.Shader]
  requiredCapabilities Rgba16i = [Capability.Shader]
  requiredCapabilities Rgba8i = [Capability.Shader]
  requiredCapabilities R32i = [Capability.Shader]
  requiredCapabilities Rg32i = [Capability.Shader]
  requiredCapabilities Rg16i = [Capability.Shader]
  requiredCapabilities Rg8i = [Capability.Shader]
  requiredCapabilities R16i = [Capability.Shader]
  requiredCapabilities R8i = [Capability.Shader]
  requiredCapabilities Rgba32ui = [Capability.Shader]
  requiredCapabilities Rgba16ui = [Capability.Shader]
  requiredCapabilities Rgba8ui = [Capability.Shader]
  requiredCapabilities R32ui = [Capability.Shader]
  requiredCapabilities Rgb10a2ui = [Capability.Shader]
  requiredCapabilities Rg32ui = [Capability.Shader]
  requiredCapabilities Rg16ui = [Capability.Shader]
  requiredCapabilities Rg8ui = [Capability.Shader]
  requiredCapabilities R16ui = [Capability.Shader]
  requiredCapabilities R8ui = [Capability.Shader]
  
