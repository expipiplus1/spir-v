{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.ImageChannelOrder where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data ImageChannelOrder = R 
                       | A 
                       | RG 
                       | RA 
                       | RGB 
                       | RGBA 
                       | BGRA 
                       | ARGB 
                       | Intensity 
                       | Luminance 
                       | Rx 
                       | RGx 
                       | RGBx 
                       | Depth 
                       | DepthStencil 
                       | SRGB 
                       | SRGBx 
                       | SRGBA 
                       | SBGRA
  deriving(Read, Show, Eq, Ord)

instance SpirEnum ImageChannelOrder Word32 where
  toWord R = 0
  toWord A = 1
  toWord RG = 2
  toWord RA = 3
  toWord RGB = 4
  toWord RGBA = 5
  toWord BGRA = 6
  toWord ARGB = 7
  toWord Intensity = 8
  toWord Luminance = 9
  toWord Rx = 10
  toWord RGx = 11
  toWord RGBx = 12
  toWord Depth = 13
  toWord DepthStencil = 14
  toWord SRGB = 15
  toWord SRGBx = 16
  toWord SRGBA = 17
  toWord SBGRA = 18

  fromWord 0 = Just R
  fromWord 1 = Just A
  fromWord 2 = Just RG
  fromWord 3 = Just RA
  fromWord 4 = Just RGB
  fromWord 5 = Just RGBA
  fromWord 6 = Just BGRA
  fromWord 7 = Just ARGB
  fromWord 8 = Just Intensity
  fromWord 9 = Just Luminance
  fromWord 10 = Just Rx
  fromWord 11 = Just RGx
  fromWord 12 = Just RGBx
  fromWord 13 = Just Depth
  fromWord 14 = Just DepthStencil
  fromWord 15 = Just SRGB
  fromWord 16 = Just SRGBx
  fromWord 17 = Just SRGBA
  fromWord 18 = Just SBGRA
  fromWord _ = Nothing

  requiredCapabilities R = [Capability.Kernel]
  requiredCapabilities A = [Capability.Kernel]
  requiredCapabilities RG = [Capability.Kernel]
  requiredCapabilities RA = [Capability.Kernel]
  requiredCapabilities RGB = [Capability.Kernel]
  requiredCapabilities RGBA = [Capability.Kernel]
  requiredCapabilities BGRA = [Capability.Kernel]
  requiredCapabilities ARGB = [Capability.Kernel]
  requiredCapabilities Intensity = [Capability.Kernel]
  requiredCapabilities Luminance = [Capability.Kernel]
  requiredCapabilities Rx = [Capability.Kernel]
  requiredCapabilities RGx = [Capability.Kernel]
  requiredCapabilities RGBx = [Capability.Kernel]
  requiredCapabilities Depth = [Capability.Kernel]
  requiredCapabilities DepthStencil = [Capability.Kernel]
  requiredCapabilities SRGB = [Capability.Kernel]
  requiredCapabilities SRGBx = [Capability.Kernel]
  requiredCapabilities SRGBA = [Capability.Kernel]
  requiredCapabilities SBGRA = [Capability.Kernel]
  
