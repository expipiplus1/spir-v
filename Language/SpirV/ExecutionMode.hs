{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.ExecutionMode where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data ExecutionMode = Invocations 
                   | SpacingEqual 
                   | SpacingFractionalEven 
                   | SpacingFractionalOdd 
                   | VertexOrderCw 
                   | VertexOrderCcw 
                   | PixelCenterInteger 
                   | OriginUpperLeft 
                   | OriginLowerLeft 
                   | EarlyFragmentTests 
                   | PointMode 
                   | Xfb 
                   | DepthReplacing 
                   | DepthAny 
                   | DepthGreater 
                   | DepthLess 
                   | DepthUnchanged 
                   | LocalSize 
                   | LocalSizeHint 
                   | InputPoints 
                   | InputLines 
                   | InputLinesAdjacency 
                   | InputTriangles 
                   | InputTrianglesAdjacency 
                   | InputQuads 
                   | InputIsolines 
                   | OutputVertices 
                   | OutputPoints 
                   | OutputLineStrip 
                   | OutputTriangleStrip 
                   | VecTypeHint 
                   | ContractionOff
  deriving(Read, Show, Eq, Ord)

instance SpirEnum ExecutionMode Word32 where
  toWord Invocations = 0
  toWord SpacingEqual = 1
  toWord SpacingFractionalEven = 2
  toWord SpacingFractionalOdd = 3
  toWord VertexOrderCw = 4
  toWord VertexOrderCcw = 5
  toWord PixelCenterInteger = 6
  toWord OriginUpperLeft = 7
  toWord OriginLowerLeft = 8
  toWord EarlyFragmentTests = 9
  toWord PointMode = 10
  toWord Xfb = 11
  toWord DepthReplacing = 12
  toWord DepthAny = 13
  toWord DepthGreater = 14
  toWord DepthLess = 15
  toWord DepthUnchanged = 16
  toWord LocalSize = 17
  toWord LocalSizeHint = 18
  toWord InputPoints = 19
  toWord InputLines = 20
  toWord InputLinesAdjacency = 21
  toWord InputTriangles = 22
  toWord InputTrianglesAdjacency = 23
  toWord InputQuads = 24
  toWord InputIsolines = 25
  toWord OutputVertices = 26
  toWord OutputPoints = 27
  toWord OutputLineStrip = 28
  toWord OutputTriangleStrip = 29
  toWord VecTypeHint = 30
  toWord ContractionOff = 31

  fromWord 0 = Just Invocations
  fromWord 1 = Just SpacingEqual
  fromWord 2 = Just SpacingFractionalEven
  fromWord 3 = Just SpacingFractionalOdd
  fromWord 4 = Just VertexOrderCw
  fromWord 5 = Just VertexOrderCcw
  fromWord 6 = Just PixelCenterInteger
  fromWord 7 = Just OriginUpperLeft
  fromWord 8 = Just OriginLowerLeft
  fromWord 9 = Just EarlyFragmentTests
  fromWord 10 = Just PointMode
  fromWord 11 = Just Xfb
  fromWord 12 = Just DepthReplacing
  fromWord 13 = Just DepthAny
  fromWord 14 = Just DepthGreater
  fromWord 15 = Just DepthLess
  fromWord 16 = Just DepthUnchanged
  fromWord 17 = Just LocalSize
  fromWord 18 = Just LocalSizeHint
  fromWord 19 = Just InputPoints
  fromWord 20 = Just InputLines
  fromWord 21 = Just InputLinesAdjacency
  fromWord 22 = Just InputTriangles
  fromWord 23 = Just InputTrianglesAdjacency
  fromWord 24 = Just InputQuads
  fromWord 25 = Just InputIsolines
  fromWord 26 = Just OutputVertices
  fromWord 27 = Just OutputPoints
  fromWord 28 = Just OutputLineStrip
  fromWord 29 = Just OutputTriangleStrip
  fromWord 30 = Just VecTypeHint
  fromWord 31 = Just ContractionOff
  fromWord _ = Nothing

  requiredCapabilities Invocations = [Capability.Geometry]
  requiredCapabilities SpacingEqual = [Capability.Tessellation]
  requiredCapabilities SpacingFractionalEven = [Capability.Tessellation]
  requiredCapabilities SpacingFractionalOdd = [Capability.Tessellation]
  requiredCapabilities VertexOrderCw = [Capability.Tessellation]
  requiredCapabilities VertexOrderCcw = [Capability.Tessellation]
  requiredCapabilities PixelCenterInteger = [Capability.Shader]
  requiredCapabilities OriginUpperLeft = [Capability.Shader]
  requiredCapabilities OriginLowerLeft = [Capability.Shader]
  requiredCapabilities EarlyFragmentTests = [Capability.Shader]
  requiredCapabilities PointMode = [Capability.Tessellation]
  requiredCapabilities Xfb = [Capability.Shader]
  requiredCapabilities DepthReplacing = [Capability.Shader]
  requiredCapabilities DepthAny = [Capability.Shader]
  requiredCapabilities DepthGreater = [Capability.Shader]
  requiredCapabilities DepthLess = [Capability.Shader]
  requiredCapabilities DepthUnchanged = [Capability.Shader]
  requiredCapabilities LocalSizeHint = [Capability.Kernel]
  requiredCapabilities InputPoints = [Capability.Geometry]
  requiredCapabilities InputLines = [Capability.Geometry]
  requiredCapabilities InputLinesAdjacency = [Capability.Geometry]
  requiredCapabilities InputTriangles = [Capability.Geometry]
  requiredCapabilities InputTrianglesAdjacency = [Capability.Geometry]
  requiredCapabilities InputQuads = [Capability.Tessellation]
  requiredCapabilities InputIsolines = [Capability.Tessellation]
  requiredCapabilities OutputVertices = [Capability.Geometry]
  requiredCapabilities OutputPoints = [Capability.Geometry]
  requiredCapabilities OutputLineStrip = [Capability.Geometry]
  requiredCapabilities OutputTriangleStrip = [Capability.Geometry]
  requiredCapabilities VecTypeHint = [Capability.Kernel]
  requiredCapabilities ContractionOff = [Capability.Kernel]
  requiredCapabilities _ = []
