{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.BuiltIn where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data BuiltIn = Position 
             | PointSize 
             | ClipDistance 
             | CullDistance 
             | VertexId 
             | InstanceId 
             | PrimitiveId 
             | InvocationId 
             | Layer 
             | ViewportIndex 
             | TessLevelOuter 
             | TessLevelInner 
             | TessCoord 
             | PatchVertices 
             | FragCoord 
             | PointCoord 
             | FrontFacing 
             | SampleId 
             | SamplePosition 
             | SampleMask 
             | FragColor 
             | FragDepth 
             | HelperInvocation 
             | NumWorkgroups 
             | WorkgroupSize 
             | WorkgroupId 
             | LocalInvocationId 
             | GlobalInvocationId 
             | LocalInvocationIndex 
             | WorkDim 
             | GlobalSize 
             | EnqueuedWorkgroupSize 
             | GlobalOffset 
             | GlobalLinearId 
             | WorkgroupLinearId 
             | SubgroupSize 
             | SubgroupMaxSize 
             | NumSubgroups 
             | NumEnqueuedSubgroups 
             | SubgroupId 
             | SubgroupLocalInvocationId
  deriving(Read, Show, Eq, Ord)

instance SpirEnum BuiltIn Word32 where
  toWord Position = 0
  toWord PointSize = 1
  toWord ClipDistance = 3
  toWord CullDistance = 4
  toWord VertexId = 5
  toWord InstanceId = 6
  toWord PrimitiveId = 7
  toWord InvocationId = 8
  toWord Layer = 9
  toWord ViewportIndex = 10
  toWord TessLevelOuter = 11
  toWord TessLevelInner = 12
  toWord TessCoord = 13
  toWord PatchVertices = 14
  toWord FragCoord = 15
  toWord PointCoord = 16
  toWord FrontFacing = 17
  toWord SampleId = 18
  toWord SamplePosition = 19
  toWord SampleMask = 20
  toWord FragColor = 21
  toWord FragDepth = 22
  toWord HelperInvocation = 23
  toWord NumWorkgroups = 24
  toWord WorkgroupSize = 25
  toWord WorkgroupId = 26
  toWord LocalInvocationId = 27
  toWord GlobalInvocationId = 28
  toWord LocalInvocationIndex = 29
  toWord WorkDim = 30
  toWord GlobalSize = 31
  toWord EnqueuedWorkgroupSize = 32
  toWord GlobalOffset = 33
  toWord GlobalLinearId = 34
  toWord WorkgroupLinearId = 35
  toWord SubgroupSize = 36
  toWord SubgroupMaxSize = 37
  toWord NumSubgroups = 38
  toWord NumEnqueuedSubgroups = 39
  toWord SubgroupId = 40
  toWord SubgroupLocalInvocationId = 41

  fromWord 0 = Just Position
  fromWord 1 = Just PointSize
  fromWord 3 = Just ClipDistance
  fromWord 4 = Just CullDistance
  fromWord 5 = Just VertexId
  fromWord 6 = Just InstanceId
  fromWord 7 = Just PrimitiveId
  fromWord 8 = Just InvocationId
  fromWord 9 = Just Layer
  fromWord 10 = Just ViewportIndex
  fromWord 11 = Just TessLevelOuter
  fromWord 12 = Just TessLevelInner
  fromWord 13 = Just TessCoord
  fromWord 14 = Just PatchVertices
  fromWord 15 = Just FragCoord
  fromWord 16 = Just PointCoord
  fromWord 17 = Just FrontFacing
  fromWord 18 = Just SampleId
  fromWord 19 = Just SamplePosition
  fromWord 20 = Just SampleMask
  fromWord 21 = Just FragColor
  fromWord 22 = Just FragDepth
  fromWord 23 = Just HelperInvocation
  fromWord 24 = Just NumWorkgroups
  fromWord 25 = Just WorkgroupSize
  fromWord 26 = Just WorkgroupId
  fromWord 27 = Just LocalInvocationId
  fromWord 28 = Just GlobalInvocationId
  fromWord 29 = Just LocalInvocationIndex
  fromWord 30 = Just WorkDim
  fromWord 31 = Just GlobalSize
  fromWord 32 = Just EnqueuedWorkgroupSize
  fromWord 33 = Just GlobalOffset
  fromWord 34 = Just GlobalLinearId
  fromWord 35 = Just WorkgroupLinearId
  fromWord 36 = Just SubgroupSize
  fromWord 37 = Just SubgroupMaxSize
  fromWord 38 = Just NumSubgroups
  fromWord 39 = Just NumEnqueuedSubgroups
  fromWord 40 = Just SubgroupId
  fromWord 41 = Just SubgroupLocalInvocationId
  fromWord _ = Nothing

  requiredCapabilities Position = [Capability.Shader]
  requiredCapabilities PointSize = [Capability.Shader]
  requiredCapabilities ClipDistance = [Capability.Shader]
  requiredCapabilities CullDistance = [Capability.Shader]
  requiredCapabilities VertexId = [Capability.Shader]
  requiredCapabilities InstanceId = [Capability.Shader]
  requiredCapabilities PrimitiveId = [Capability.Geometry]
  requiredCapabilities InvocationId = [Capability.Geometry]
  requiredCapabilities Layer = [Capability.Geometry]
  requiredCapabilities ViewportIndex = [Capability.Geometry]
  requiredCapabilities TessLevelOuter = [Capability.Tessellation]
  requiredCapabilities TessLevelInner = [Capability.Tessellation]
  requiredCapabilities TessCoord = [Capability.Tessellation]
  requiredCapabilities PatchVertices = [Capability.Tessellation]
  requiredCapabilities FragCoord = [Capability.Shader]
  requiredCapabilities PointCoord = [Capability.Shader]
  requiredCapabilities FrontFacing = [Capability.Shader]
  requiredCapabilities SampleId = [Capability.Shader]
  requiredCapabilities SamplePosition = [Capability.Shader]
  requiredCapabilities SampleMask = [Capability.Shader]
  requiredCapabilities FragColor = [Capability.Shader]
  requiredCapabilities FragDepth = [Capability.Shader]
  requiredCapabilities HelperInvocation = [Capability.Shader]
  requiredCapabilities LocalInvocationIndex = [Capability.Shader]
  requiredCapabilities WorkDim = [Capability.Kernel]
  requiredCapabilities GlobalSize = [Capability.Kernel]
  requiredCapabilities EnqueuedWorkgroupSize = [Capability.Kernel]
  requiredCapabilities GlobalOffset = [Capability.Kernel]
  requiredCapabilities GlobalLinearId = [Capability.Kernel]
  requiredCapabilities WorkgroupLinearId = [Capability.Kernel]
  requiredCapabilities SubgroupSize = [Capability.Kernel]
  requiredCapabilities SubgroupMaxSize = [Capability.Kernel]
  requiredCapabilities NumSubgroups = [Capability.Kernel]
  requiredCapabilities NumEnqueuedSubgroups = [Capability.Kernel]
  requiredCapabilities SubgroupId = [Capability.Kernel]
  requiredCapabilities SubgroupLocalInvocationId = [Capability.Kernel]
  requiredCapabilities _ = []
