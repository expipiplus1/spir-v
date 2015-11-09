{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.ExecutionModel where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data ExecutionModel = Vertex 
                    | TessellationControl 
                    | TessellationEvaluation 
                    | Geometry 
                    | Fragment 
                    | GLCompute 
                    | Kernel
  deriving(Read, Show, Eq, Ord)

instance SpirEnum ExecutionModel Word32 where
  toWord Vertex = 0
  toWord TessellationControl = 1
  toWord TessellationEvaluation = 2
  toWord Geometry = 3
  toWord Fragment = 4
  toWord GLCompute = 5
  toWord Kernel = 6

  fromWord 0 = Just Vertex
  fromWord 1 = Just TessellationControl
  fromWord 2 = Just TessellationEvaluation
  fromWord 3 = Just Geometry
  fromWord 4 = Just Fragment
  fromWord 5 = Just GLCompute
  fromWord 6 = Just Kernel
  fromWord _ = Nothing

  requiredCapabilities Vertex = [Capability.Shader]
  requiredCapabilities TessellationControl = [Capability.Tessellation]
  requiredCapabilities TessellationEvaluation = [Capability.Tessellation]
  requiredCapabilities Geometry = [Capability.Geometry]
  requiredCapabilities Fragment = [Capability.Shader]
  requiredCapabilities GLCompute = [Capability.Shader]
  requiredCapabilities Kernel = [Capability.Kernel]
  
