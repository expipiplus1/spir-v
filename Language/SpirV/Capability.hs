{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.Capability where

import Data.Word (Word32)
import Language.SpirV.SpirEnum

data Capability = Matrix 
                | Shader 
                | Geometry 
                | Tessellation 
                | Addresses 
                | Linkage 
                | Kernel 
                | Vector16 
                | Float16Buffer 
                | Float16 
                | Float64 
                | Int64 
                | Int64Atomics 
                | ImageBasic 
                | ImageReadWrite 
                | ImageMipmap 
                | ImageSRGBWrite 
                | Pipes 
                | Groups 
                | DeviceEnqueue 
                | LiteralSampler 
                | AtomicStorage 
                | Int16 
                | TessellationPointSize 
                | GeometryPointSize 
                | ImageGatherExtended 
                | StorageImageExtendedFormats 
                | StorageImageMultisample 
                | UniformBufferArrayDynamicIndexing 
                | SampledImageArrayDynamicIndexing 
                | StorageBufferArrayDynamicIndexing 
                | StorageImageArrayDynamicIndexing 
                | ClipDistance 
                | CullDistance 
                | ImageCubeArray 
                | SampleRateShading
  deriving(Read, Show, Eq, Ord)

instance SpirEnum Capability Word32 where
  toWord Matrix = 0
  toWord Shader = 1
  toWord Geometry = 2
  toWord Tessellation = 3
  toWord Addresses = 4
  toWord Linkage = 5
  toWord Kernel = 6
  toWord Vector16 = 7
  toWord Float16Buffer = 8
  toWord Float16 = 9
  toWord Float64 = 10
  toWord Int64 = 11
  toWord Int64Atomics = 12
  toWord ImageBasic = 13
  toWord ImageReadWrite = 14
  toWord ImageMipmap = 15
  toWord ImageSRGBWrite = 16
  toWord Pipes = 17
  toWord Groups = 18
  toWord DeviceEnqueue = 19
  toWord LiteralSampler = 20
  toWord AtomicStorage = 21
  toWord Int16 = 22
  toWord TessellationPointSize = 23
  toWord GeometryPointSize = 24
  toWord ImageGatherExtended = 25
  toWord StorageImageExtendedFormats = 26
  toWord StorageImageMultisample = 27
  toWord UniformBufferArrayDynamicIndexing = 28
  toWord SampledImageArrayDynamicIndexing = 29
  toWord StorageBufferArrayDynamicIndexing = 30
  toWord StorageImageArrayDynamicIndexing = 31
  toWord ClipDistance = 32
  toWord CullDistance = 33
  toWord ImageCubeArray = 34
  toWord SampleRateShading = 35

  fromWord 0 = Just Matrix
  fromWord 1 = Just Shader
  fromWord 2 = Just Geometry
  fromWord 3 = Just Tessellation
  fromWord 4 = Just Addresses
  fromWord 5 = Just Linkage
  fromWord 6 = Just Kernel
  fromWord 7 = Just Vector16
  fromWord 8 = Just Float16Buffer
  fromWord 9 = Just Float16
  fromWord 10 = Just Float64
  fromWord 11 = Just Int64
  fromWord 12 = Just Int64Atomics
  fromWord 13 = Just ImageBasic
  fromWord 14 = Just ImageReadWrite
  fromWord 15 = Just ImageMipmap
  fromWord 16 = Just ImageSRGBWrite
  fromWord 17 = Just Pipes
  fromWord 18 = Just Groups
  fromWord 19 = Just DeviceEnqueue
  fromWord 20 = Just LiteralSampler
  fromWord 21 = Just AtomicStorage
  fromWord 22 = Just Int16
  fromWord 23 = Just TessellationPointSize
  fromWord 24 = Just GeometryPointSize
  fromWord 25 = Just ImageGatherExtended
  fromWord 26 = Just StorageImageExtendedFormats
  fromWord 27 = Just StorageImageMultisample
  fromWord 28 = Just UniformBufferArrayDynamicIndexing
  fromWord 29 = Just SampledImageArrayDynamicIndexing
  fromWord 30 = Just StorageBufferArrayDynamicIndexing
  fromWord 31 = Just StorageImageArrayDynamicIndexing
  fromWord 32 = Just ClipDistance
  fromWord 33 = Just CullDistance
  fromWord 34 = Just ImageCubeArray
  fromWord 35 = Just SampleRateShading
  fromWord _ = Nothing

  requiredCapabilities Shader = [Matrix]
  requiredCapabilities Geometry = [Shader]
  requiredCapabilities Tessellation = [Shader]
  requiredCapabilities _ = []
