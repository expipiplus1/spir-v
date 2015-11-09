{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.Decoration where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data Decoration = RelaxedPrecision 
                | SpecId 
                | Block 
                | BufferBlock 
                | RowMajor 
                | ColMajor 
                | ArrayStride 
                | MatrixStride 
                | GLSLShared 
                | GLSLPacked 
                | CPacked 
                | BuiltIn 
                | Smooth 
                | Noperspective 
                | Flat 
                | Patch 
                | Centroid 
                | Sample 
                | Invariant 
                | Restrict 
                | Aliased 
                | Volatile 
                | Constant 
                | Coherent 
                | Nonwritable 
                | Nonreadable 
                | Uniform 
                | NoStaticUse 
                | SaturatedConversion 
                | Stream 
                | Location 
                | Component 
                | Index 
                | Binding 
                | DescriptorSet 
                | Offset 
                | XfbBuffer 
                | XfbStride 
                | FuncParamAttr 
                | FPRoundingMode 
                | FPFastMathMode 
                | LinkageAttributes
  deriving(Read, Show, Eq, Ord)

instance SpirEnum Decoration Word32 where
  toWord RelaxedPrecision = 0
  toWord SpecId = 1
  toWord Block = 2
  toWord BufferBlock = 3
  toWord RowMajor = 4
  toWord ColMajor = 5
  toWord ArrayStride = 6
  toWord MatrixStride = 7
  toWord GLSLShared = 8
  toWord GLSLPacked = 9
  toWord CPacked = 10
  toWord BuiltIn = 11
  toWord Smooth = 12
  toWord Noperspective = 13
  toWord Flat = 14
  toWord Patch = 15
  toWord Centroid = 16
  toWord Sample = 17
  toWord Invariant = 18
  toWord Restrict = 19
  toWord Aliased = 20
  toWord Volatile = 21
  toWord Constant = 22
  toWord Coherent = 23
  toWord Nonwritable = 24
  toWord Nonreadable = 25
  toWord Uniform = 26
  toWord NoStaticUse = 27
  toWord SaturatedConversion = 28
  toWord Stream = 29
  toWord Location = 30
  toWord Component = 31
  toWord Index = 32
  toWord Binding = 33
  toWord DescriptorSet = 34
  toWord Offset = 35
  toWord XfbBuffer = 36
  toWord XfbStride = 37
  toWord FuncParamAttr = 38
  toWord FPRoundingMode = 39
  toWord FPFastMathMode = 40
  toWord LinkageAttributes = 41

  fromWord 0 = Just RelaxedPrecision
  fromWord 1 = Just SpecId
  fromWord 2 = Just Block
  fromWord 3 = Just BufferBlock
  fromWord 4 = Just RowMajor
  fromWord 5 = Just ColMajor
  fromWord 6 = Just ArrayStride
  fromWord 7 = Just MatrixStride
  fromWord 8 = Just GLSLShared
  fromWord 9 = Just GLSLPacked
  fromWord 10 = Just CPacked
  fromWord 11 = Just BuiltIn
  fromWord 12 = Just Smooth
  fromWord 13 = Just Noperspective
  fromWord 14 = Just Flat
  fromWord 15 = Just Patch
  fromWord 16 = Just Centroid
  fromWord 17 = Just Sample
  fromWord 18 = Just Invariant
  fromWord 19 = Just Restrict
  fromWord 20 = Just Aliased
  fromWord 21 = Just Volatile
  fromWord 22 = Just Constant
  fromWord 23 = Just Coherent
  fromWord 24 = Just Nonwritable
  fromWord 25 = Just Nonreadable
  fromWord 26 = Just Uniform
  fromWord 27 = Just NoStaticUse
  fromWord 28 = Just SaturatedConversion
  fromWord 29 = Just Stream
  fromWord 30 = Just Location
  fromWord 31 = Just Component
  fromWord 32 = Just Index
  fromWord 33 = Just Binding
  fromWord 34 = Just DescriptorSet
  fromWord 35 = Just Offset
  fromWord 36 = Just XfbBuffer
  fromWord 37 = Just XfbStride
  fromWord 38 = Just FuncParamAttr
  fromWord 39 = Just FPRoundingMode
  fromWord 40 = Just FPFastMathMode
  fromWord 41 = Just LinkageAttributes
  fromWord _ = Nothing

  requiredCapabilities RelaxedPrecision = [Capability.Shader]
  requiredCapabilities SpecId = [Capability.Shader]
  requiredCapabilities Block = [Capability.Shader]
  requiredCapabilities BufferBlock = [Capability.Shader]
  requiredCapabilities RowMajor = [Capability.Matrix]
  requiredCapabilities ColMajor = [Capability.Matrix]
  requiredCapabilities ArrayStride = [Capability.Shader]
  requiredCapabilities MatrixStride = [Capability.Shader]
  requiredCapabilities GLSLShared = [Capability.Shader]
  requiredCapabilities GLSLPacked = [Capability.Shader]
  requiredCapabilities CPacked = [Capability.Kernel]
  requiredCapabilities BuiltIn = [Capability.Shader]
  requiredCapabilities Smooth = [Capability.Shader]
  requiredCapabilities Noperspective = [Capability.Shader]
  requiredCapabilities Flat = [Capability.Shader]
  requiredCapabilities Patch = [Capability.Tessellation]
  requiredCapabilities Centroid = [Capability.Shader]
  requiredCapabilities Sample = [Capability.Shader]
  requiredCapabilities Invariant = [Capability.Shader]
  requiredCapabilities Constant = [Capability.Kernel]
  requiredCapabilities Uniform = [Capability.Shader]
  requiredCapabilities SaturatedConversion = [Capability.Kernel]
  requiredCapabilities Stream = [Capability.Geometry]
  requiredCapabilities Location = [Capability.Shader]
  requiredCapabilities Component = [Capability.Shader]
  requiredCapabilities Index = [Capability.Shader]
  requiredCapabilities Binding = [Capability.Shader]
  requiredCapabilities DescriptorSet = [Capability.Shader]
  requiredCapabilities XfbBuffer = [Capability.Shader]
  requiredCapabilities XfbStride = [Capability.Shader]
  requiredCapabilities FuncParamAttr = [Capability.Kernel]
  requiredCapabilities FPRoundingMode = [Capability.Kernel]
  requiredCapabilities FPFastMathMode = [Capability.Kernel]
  requiredCapabilities LinkageAttributes = [Capability.Linkage]
  requiredCapabilities _ = []
