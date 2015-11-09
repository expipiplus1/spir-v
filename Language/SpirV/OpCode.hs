{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.OpCode where

import           Data.Word (Word16)
import qualified Language.SpirV.Capability as Capability
import           Language.SpirV.SpirEnum

data OpCode = OpNop 
            | OpUndef 
            | OpSource 
            | OpSourceExtension 
            | OpName 
            | OpMemberName 
            | OpString 
            | OpLine 
            | OpDecorate 
            | OpMemberDecorate 
            | OpDecorationGroup 
            | OpGroupDecorate 
            | OpGroupMemberDecorate 
            | OpExtension 
            | OpExtInstImport 
            | OpExtInst 
            | OpMemoryModel 
            | OpEntryPoint 
            | OpExecutionMode 
            | OpCapability 
            | OpTypeVoid 
            | OpTypeBool 
            | OpTypeInt 
            | OpTypeFloat 
            | OpTypeVector 
            | OpTypeMatrix 
            | OpTypeImage 
            | OpTypeSampler 
            | OpTypeSampledImage 
            | OpTypeArray 
            | OpTypeRuntimeArray 
            | OpTypeStruct 
            | OpTypeOpaque 
            | OpTypePointer 
            | OpTypeFunction 
            | OpTypeEvent 
            | OpTypeDeviceEvent 
            | OpTypeReserveId 
            | OpTypeQueue 
            | OpTypePipe 
            | OpConstantTrue 
            | OpConstantFalse 
            | OpConstant 
            | OpConstantComposite 
            | OpConstantSampler 
            | OpConstantNull 
            | OpSpecConstantTrue 
            | OpSpecConstantFalse 
            | OpSpecConstant 
            | OpSpecConstantComposite 
            | OpSpecConstantOp 
            | OpVariable 
            | OpImageTexelPointer 
            | OpLoad 
            | OpStore 
            | OpCopyMemory 
            | OpCopyMemorySized 
            | OpAccessChain 
            | OpInBoundsAccessChain 
            | OpPtrAccessChain 
            | OpArrayLength 
            | OpGenericPtrMemSemantics 
            | OpFunction 
            | OpFunctionParameter 
            | OpFunctionEnd 
            | OpFunctionCall 
            | OpSampledImage 
            | OpImageSampleImplicitLod 
            | OpImageSampleExplicitLod 
            | OpImageSampleDrefImplicitLod 
            | OpImageSampleDrefExplicitLod 
            | OpImageSampleProjImplicitLod 
            | OpImageSampleProjExplicitLod 
            | OpImageSampleProjDrefImplicitLod 
            | OpImageSampleProjDrefExplicitLod 
            | OpImageFetch 
            | OpImageGather 
            | OpImageDrefGather 
            | OpImageRead 
            | OpImageWrite 
            | OpImageQueryDim 
            | OpImageQueryFormat 
            | OpImageQueryOrder 
            | OpImageQuerySizeLod 
            | OpImageQuerySize 
            | OpImageQueryLod 
            | OpImageQueryLevels 
            | OpImageQuerySamples 
            | OpConvertFToU 
            | OpConvertFToS 
            | OpConvertSToF 
            | OpConvertUToF 
            | OpUConvert 
            | OpSConvert 
            | OpFConvert 
            | OpQuantizeToF16 
            | OpConvertPtrToU 
            | OpSatConvertSToU 
            | OpSatConvertUToS 
            | OpConvertUToPtr 
            | OpPtrCastToGeneric 
            | OpGenericCastToPtr 
            | OpGenericCastToPtrExplicit 
            | OpBitcast 
            | OpVectorExtractDynamic 
            | OpVectorInsertDynamic 
            | OpVectorShuffle 
            | OpCompositeConstruct 
            | OpCompositeExtract 
            | OpCompositeInsert 
            | OpCopyObject 
            | OpTranspose 
            | OpSNegate 
            | OpFNegate 
            | OpIAdd 
            | OpFAdd 
            | OpISub 
            | OpFSub 
            | OpIMul 
            | OpFMul 
            | OpUDiv 
            | OpSDiv 
            | OpFDiv 
            | OpUMod 
            | OpSRem 
            | OpSMod 
            | OpFRem 
            | OpFMod 
            | OpVectorTimesScalar 
            | OpMatrixTimesScalar 
            | OpVectorTimesMatrix 
            | OpMatrixTimesVector 
            | OpMatrixTimesMatrix 
            | OpOuterProduct 
            | OpDot 
            | OpIAddCarry 
            | OpISubBorrow 
            | OpIMulExtended 
            | OpShiftRightLogical 
            | OpShiftRightArithmetic 
            | OpShiftLeftLogical 
            | OpBitwiseOr 
            | OpBitwiseXor 
            | OpBitwiseAnd 
            | OpNot 
            | OpBitFieldInsert 
            | OpBitFieldSExtract 
            | OpBitFieldUExtract 
            | OpBitReverse 
            | OpBitCount 
            | OpAny 
            | OpAll 
            | OpIsNan 
            | OpIsInf 
            | OpIsFinite 
            | OpIsNormal 
            | OpSignBitSet 
            | OpLessOrGreater 
            | OpOrdered 
            | OpUnordered 
            | OpLogicalEqual 
            | OpLogicalNotEqual 
            | OpLogicalOr 
            | OpLogicalAnd 
            | OpLogicalNot 
            | OpSelect 
            | OpIEqual 
            | OpINotEqual 
            | OpUGreaterThan 
            | OpSGreaterThan 
            | OpUGreaterThanEqual 
            | OpSGreaterThanEqual 
            | OpULessThan 
            | OpSLessThan 
            | OpULessThanEqual 
            | OpSLessThanEqual 
            | OpFOrdEqual 
            | OpFUnordEqual 
            | OpFOrdNotEqual 
            | OpFUnordNotEqual 
            | OpFOrdLessThan 
            | OpFUnordLessThan 
            | OpFOrdGreaterThan 
            | OpFUnordGreaterThan 
            | OpFOrdLessThanEqual 
            | OpFUnordLessThanEqual 
            | OpFOrdGreaterThanEqual 
            | OpFUnordGreaterThanEqual 
            | OpDPdx 
            | OpDPdy 
            | OpFwidth 
            | OpDPdxFine 
            | OpDPdyFine 
            | OpFwidthFine 
            | OpDPdxCoarse 
            | OpDPdyCoarse 
            | OpFwidthCoarse 
            | OpPhi 
            | OpLoopMerge 
            | OpSelectionMerge 
            | OpLabel 
            | OpBranch 
            | OpBranchConditional 
            | OpSwitch 
            | OpKill 
            | OpReturn 
            | OpReturnValue 
            | OpUnreachable 
            | OpLifetimeStart 
            | OpLifetimeStop 
            | OpAtomicLoad 
            | OpAtomicStore 
            | OpAtomicExchange 
            | OpAtomicCompareExchange 
            | OpAtomicCompareExchangeWeak 
            | OpAtomicIIncrement 
            | OpAtomicIDecrement 
            | OpAtomicIAdd 
            | OpAtomicISub 
            | OpAtomicSMin 
            | OpAtomicUMin 
            | OpAtomicSMax 
            | OpAtomicUMax 
            | OpAtomicAnd 
            | OpAtomicOr 
            | OpAtomicXor 
            | OpEmitVertex 
            | OpEndPrimitive 
            | OpEmitStreamVertex 
            | OpEndStreamPrimitive 
            | OpControlBarrier 
            | OpMemoryBarrier 
            | OpAsyncGroupCopy 
            | OpWaitGroupEvents 
            | OpGroupAll 
            | OpGroupAny 
            | OpGroupBroadcast 
            | OpGroupIAdd 
            | OpGroupFAdd 
            | OpGroupFMin 
            | OpGroupUMin 
            | OpGroupSMin 
            | OpGroupFMax 
            | OpGroupUMax 
            | OpGroupSMax 
            | OpEnqueueMarker 
            | OpEnqueueKernel 
            | OpGetKernelNDrangeSubGroupCount 
            | OpGetKernelNDrangeMaxSubGroupSize 
            | OpGetKernelWorkGroupSize 
            | OpGetKernelPreferredWorkGroupSizeMultiple 
            | OpRetainEvent 
            | OpReleaseEvent 
            | OpCreateUserEvent 
            | OpIsValidEvent 
            | OpSetUserEventStatus 
            | OpCaptureEventProfilingInfo 
            | OpGetDefaultQueue 
            | OpBuildNDRange 
            | OpReadPipe 
            | OpWritePipe 
            | OpReservedReadPipe 
            | OpReservedWritePipe 
            | OpReserveReadPipePackets 
            | OpReserveWritePipePackets 
            | OpCommitReadPipe 
            | OpCommitWritePipe 
            | OpIsValidReserveId 
            | OpGetNumPipePackets 
            | OpGetMaxPipePackets 
            | OpGroupReserveReadPipePackets 
            | OpGroupReserveWritePipePackets 
            | OpGroupCommitReadPipe 
            | OpGroupCommitWritePipe
  deriving(Read, Show, Eq, Ord)

data InstructionSize = Fixed Word16
                     | Variadic Word16
  deriving(Read, Show, Eq)

instructionSize :: OpCode -> InstructionSize
instructionSize OpNop = Fixed 1
instructionSize OpUndef = Fixed 3
instructionSize OpSource = Fixed 3
instructionSize OpSourceExtension = Variadic 1
instructionSize OpName = Variadic 2
instructionSize OpMemberName = Variadic 3
instructionSize OpString = Variadic 2
instructionSize OpLine = Fixed 5
instructionSize OpDecorate = Variadic 3
instructionSize OpMemberDecorate = Variadic 4
instructionSize OpDecorationGroup = Fixed 2
instructionSize OpGroupDecorate = Variadic 2
instructionSize OpGroupMemberDecorate = Fixed 2
instructionSize OpExtension = Variadic 1
instructionSize OpExtInstImport = Variadic 2
instructionSize OpExtInst = Variadic 5
instructionSize OpMemoryModel = Fixed 3
instructionSize OpEntryPoint = Variadic 3
instructionSize OpExecutionMode = Fixed 3
instructionSize OpCapability = Fixed 2
instructionSize OpTypeVoid = Fixed 2
instructionSize OpTypeBool = Fixed 2
instructionSize OpTypeInt = Fixed 4
instructionSize OpTypeFloat = Fixed 3
instructionSize OpTypeVector = Fixed 4
instructionSize OpTypeMatrix = Fixed 4
instructionSize OpTypeImage = Fixed 9
instructionSize OpTypeSampler = Fixed 2
instructionSize OpTypeSampledImage = Fixed 3
instructionSize OpTypeArray = Fixed 4
instructionSize OpTypeRuntimeArray = Fixed 3
instructionSize OpTypeStruct = Variadic 2
instructionSize OpTypeOpaque = Variadic 2
instructionSize OpTypePointer = Fixed 4
instructionSize OpTypeFunction = Variadic 3
instructionSize OpTypeEvent = Fixed 2
instructionSize OpTypeDeviceEvent = Fixed 2
instructionSize OpTypeReserveId = Fixed 2
instructionSize OpTypeQueue = Fixed 2
instructionSize OpTypePipe = Fixed 4
instructionSize OpConstantTrue = Fixed 3
instructionSize OpConstantFalse = Fixed 3
instructionSize OpConstant = Variadic 3
instructionSize OpConstantComposite = Variadic 3
instructionSize OpConstantSampler = Fixed 6
instructionSize OpConstantNull = Fixed 3
instructionSize OpSpecConstantTrue = Fixed 3
instructionSize OpSpecConstantFalse = Fixed 3
instructionSize OpSpecConstant = Variadic 3
instructionSize OpSpecConstantComposite = Variadic 3
instructionSize OpSpecConstantOp = Variadic 4
instructionSize OpVariable = Variadic 4
instructionSize OpImageTexelPointer = Fixed 6
instructionSize OpLoad = Fixed 4
instructionSize OpStore = Fixed 3
instructionSize OpCopyMemory = Fixed 3
instructionSize OpCopyMemorySized = Fixed 4
instructionSize OpAccessChain = Variadic 4
instructionSize OpInBoundsAccessChain = Variadic 4
instructionSize OpPtrAccessChain = Variadic 5
instructionSize OpArrayLength = Fixed 5
instructionSize OpGenericPtrMemSemantics = Fixed 4
instructionSize OpFunction = Fixed 5
instructionSize OpFunctionParameter = Fixed 3
instructionSize OpFunctionEnd = Fixed 1
instructionSize OpFunctionCall = Variadic 4
instructionSize OpSampledImage = Fixed 5
instructionSize OpImageSampleImplicitLod = Variadic 5
instructionSize OpImageSampleExplicitLod = Variadic 5
instructionSize OpImageSampleDrefImplicitLod = Variadic 6
instructionSize OpImageSampleDrefExplicitLod = Variadic 6
instructionSize OpImageSampleProjImplicitLod = Variadic 5
instructionSize OpImageSampleProjExplicitLod = Variadic 5
instructionSize OpImageSampleProjDrefImplicitLod = Variadic 6
instructionSize OpImageSampleProjDrefExplicitLod = Variadic 6
instructionSize OpImageFetch = Variadic 5
instructionSize OpImageGather = Variadic 6
instructionSize OpImageDrefGather = Variadic 6
instructionSize OpImageRead = Fixed 5
instructionSize OpImageWrite = Fixed 4
instructionSize OpImageQueryDim = Fixed 4
instructionSize OpImageQueryFormat = Fixed 4
instructionSize OpImageQueryOrder = Fixed 4
instructionSize OpImageQuerySizeLod = Fixed 5
instructionSize OpImageQuerySize = Fixed 4
instructionSize OpImageQueryLod = Fixed 5
instructionSize OpImageQueryLevels = Fixed 4
instructionSize OpImageQuerySamples = Fixed 4
instructionSize OpConvertFToU = Fixed 4
instructionSize OpConvertFToS = Fixed 4
instructionSize OpConvertSToF = Fixed 4
instructionSize OpConvertUToF = Fixed 4
instructionSize OpUConvert = Fixed 4
instructionSize OpSConvert = Fixed 4
instructionSize OpFConvert = Fixed 4
instructionSize OpQuantizeToF16 = Fixed 4
instructionSize OpConvertPtrToU = Fixed 4
instructionSize OpSatConvertSToU = Fixed 4
instructionSize OpSatConvertUToS = Fixed 4
instructionSize OpConvertUToPtr = Fixed 4
instructionSize OpPtrCastToGeneric = Fixed 4
instructionSize OpGenericCastToPtr = Fixed 4
instructionSize OpGenericCastToPtrExplicit = Fixed 5
instructionSize OpBitcast = Fixed 4
instructionSize OpVectorExtractDynamic = Fixed 5
instructionSize OpVectorInsertDynamic = Fixed 6
instructionSize OpVectorShuffle = Variadic 5
instructionSize OpCompositeConstruct = Variadic 3
instructionSize OpCompositeExtract = Variadic 4
instructionSize OpCompositeInsert = Variadic 5
instructionSize OpCopyObject = Fixed 4
instructionSize OpTranspose = Fixed 4
instructionSize OpSNegate = Fixed 4
instructionSize OpFNegate = Fixed 4
instructionSize OpIAdd = Fixed 5
instructionSize OpFAdd = Fixed 5
instructionSize OpISub = Fixed 5
instructionSize OpFSub = Fixed 5
instructionSize OpIMul = Fixed 5
instructionSize OpFMul = Fixed 5
instructionSize OpUDiv = Fixed 5
instructionSize OpSDiv = Fixed 5
instructionSize OpFDiv = Fixed 5
instructionSize OpUMod = Fixed 5
instructionSize OpSRem = Fixed 5
instructionSize OpSMod = Fixed 5
instructionSize OpFRem = Fixed 5
instructionSize OpFMod = Fixed 5
instructionSize OpVectorTimesScalar = Fixed 5
instructionSize OpMatrixTimesScalar = Fixed 5
instructionSize OpVectorTimesMatrix = Fixed 5
instructionSize OpMatrixTimesVector = Fixed 5
instructionSize OpMatrixTimesMatrix = Fixed 5
instructionSize OpOuterProduct = Fixed 5
instructionSize OpDot = Fixed 5
instructionSize OpIAddCarry = Fixed 3
instructionSize OpISubBorrow = Fixed 3
instructionSize OpIMulExtended = Fixed 3
instructionSize OpShiftRightLogical = Fixed 5
instructionSize OpShiftRightArithmetic = Fixed 5
instructionSize OpShiftLeftLogical = Fixed 5
instructionSize OpBitwiseOr = Fixed 5
instructionSize OpBitwiseXor = Fixed 5
instructionSize OpBitwiseAnd = Fixed 5
instructionSize OpNot = Fixed 4
instructionSize OpBitFieldInsert = Fixed 7
instructionSize OpBitFieldSExtract = Fixed 6
instructionSize OpBitFieldUExtract = Fixed 6
instructionSize OpBitReverse = Fixed 4
instructionSize OpBitCount = Fixed 4
instructionSize OpAny = Fixed 4
instructionSize OpAll = Fixed 4
instructionSize OpIsNan = Fixed 4
instructionSize OpIsInf = Fixed 4
instructionSize OpIsFinite = Fixed 4
instructionSize OpIsNormal = Fixed 4
instructionSize OpSignBitSet = Fixed 4
instructionSize OpLessOrGreater = Fixed 5
instructionSize OpOrdered = Fixed 5
instructionSize OpUnordered = Fixed 5
instructionSize OpLogicalEqual = Fixed 5
instructionSize OpLogicalNotEqual = Fixed 5
instructionSize OpLogicalOr = Fixed 5
instructionSize OpLogicalAnd = Fixed 5
instructionSize OpLogicalNot = Fixed 4
instructionSize OpSelect = Fixed 6
instructionSize OpIEqual = Fixed 5
instructionSize OpINotEqual = Fixed 5
instructionSize OpUGreaterThan = Fixed 5
instructionSize OpSGreaterThan = Fixed 5
instructionSize OpUGreaterThanEqual = Fixed 5
instructionSize OpSGreaterThanEqual = Fixed 5
instructionSize OpULessThan = Fixed 5
instructionSize OpSLessThan = Fixed 5
instructionSize OpULessThanEqual = Fixed 5
instructionSize OpSLessThanEqual = Fixed 5
instructionSize OpFOrdEqual = Fixed 5
instructionSize OpFUnordEqual = Fixed 5
instructionSize OpFOrdNotEqual = Fixed 5
instructionSize OpFUnordNotEqual = Fixed 5
instructionSize OpFOrdLessThan = Fixed 5
instructionSize OpFUnordLessThan = Fixed 5
instructionSize OpFOrdGreaterThan = Fixed 5
instructionSize OpFUnordGreaterThan = Fixed 5
instructionSize OpFOrdLessThanEqual = Fixed 5
instructionSize OpFUnordLessThanEqual = Fixed 5
instructionSize OpFOrdGreaterThanEqual = Fixed 5
instructionSize OpFUnordGreaterThanEqual = Fixed 5
instructionSize OpDPdx = Fixed 4
instructionSize OpDPdy = Fixed 4
instructionSize OpFwidth = Fixed 4
instructionSize OpDPdxFine = Fixed 4
instructionSize OpDPdyFine = Fixed 4
instructionSize OpFwidthFine = Fixed 4
instructionSize OpDPdxCoarse = Fixed 4
instructionSize OpDPdyCoarse = Fixed 4
instructionSize OpFwidthCoarse = Fixed 4
instructionSize OpPhi = Variadic 3
instructionSize OpLoopMerge = Fixed 3
instructionSize OpSelectionMerge = Fixed 3
instructionSize OpLabel = Fixed 2
instructionSize OpBranch = Fixed 2
instructionSize OpBranchConditional = Variadic 4
instructionSize OpSwitch = Fixed 3
instructionSize OpKill = Fixed 1
instructionSize OpReturn = Fixed 1
instructionSize OpReturnValue = Fixed 2
instructionSize OpUnreachable = Fixed 1
instructionSize OpLifetimeStart = Fixed 3
instructionSize OpLifetimeStop = Fixed 3
instructionSize OpAtomicLoad = Fixed 6
instructionSize OpAtomicStore = Fixed 5
instructionSize OpAtomicExchange = Fixed 7
instructionSize OpAtomicCompareExchange = Fixed 9
instructionSize OpAtomicCompareExchangeWeak = Fixed 9
instructionSize OpAtomicIIncrement = Fixed 6
instructionSize OpAtomicIDecrement = Fixed 6
instructionSize OpAtomicIAdd = Fixed 7
instructionSize OpAtomicISub = Fixed 7
instructionSize OpAtomicSMin = Fixed 7
instructionSize OpAtomicUMin = Fixed 7
instructionSize OpAtomicSMax = Fixed 7
instructionSize OpAtomicUMax = Fixed 7
instructionSize OpAtomicAnd = Fixed 7
instructionSize OpAtomicOr = Fixed 7
instructionSize OpAtomicXor = Fixed 7
instructionSize OpEmitVertex = Fixed 1
instructionSize OpEndPrimitive = Fixed 1
instructionSize OpEmitStreamVertex = Fixed 2
instructionSize OpEndStreamPrimitive = Fixed 2
instructionSize OpControlBarrier = Fixed 4
instructionSize OpMemoryBarrier = Fixed 3
instructionSize OpAsyncGroupCopy = Fixed 9
instructionSize OpWaitGroupEvents = Fixed 4
instructionSize OpGroupAll = Fixed 5
instructionSize OpGroupAny = Fixed 5
instructionSize OpGroupBroadcast = Fixed 6
instructionSize OpGroupIAdd = Fixed 6
instructionSize OpGroupFAdd = Fixed 6
instructionSize OpGroupFMin = Fixed 6
instructionSize OpGroupUMin = Fixed 6
instructionSize OpGroupSMin = Fixed 6
instructionSize OpGroupFMax = Fixed 6
instructionSize OpGroupUMax = Fixed 6
instructionSize OpGroupSMax = Fixed 6
instructionSize OpEnqueueMarker = Fixed 7
instructionSize OpEnqueueKernel = Variadic 13
instructionSize OpGetKernelNDrangeSubGroupCount = Fixed 8
instructionSize OpGetKernelNDrangeMaxSubGroupSize = Fixed 8
instructionSize OpGetKernelWorkGroupSize = Fixed 7
instructionSize OpGetKernelPreferredWorkGroupSizeMultiple = Fixed 7
instructionSize OpRetainEvent = Fixed 2
instructionSize OpReleaseEvent = Fixed 2
instructionSize OpCreateUserEvent = Fixed 3
instructionSize OpIsValidEvent = Fixed 4
instructionSize OpSetUserEventStatus = Fixed 3
instructionSize OpCaptureEventProfilingInfo = Fixed 4
instructionSize OpGetDefaultQueue = Fixed 3
instructionSize OpBuildNDRange = Fixed 6
instructionSize OpReadPipe = Fixed 5
instructionSize OpWritePipe = Fixed 5
instructionSize OpReservedReadPipe = Fixed 7
instructionSize OpReservedWritePipe = Fixed 7
instructionSize OpReserveReadPipePackets = Fixed 5
instructionSize OpReserveWritePipePackets = Fixed 5
instructionSize OpCommitReadPipe = Fixed 3
instructionSize OpCommitWritePipe = Fixed 3
instructionSize OpIsValidReserveId = Fixed 4
instructionSize OpGetNumPipePackets = Fixed 4
instructionSize OpGetMaxPipePackets = Fixed 4
instructionSize OpGroupReserveReadPipePackets = Fixed 6
instructionSize OpGroupReserveWritePipePackets = Fixed 6
instructionSize OpGroupCommitReadPipe = Fixed 4
instructionSize OpGroupCommitWritePipe = Fixed 4

instance SpirEnum OpCode Word16 where
  toWord OpNop = 0
  toWord OpUndef = 1
  toWord OpSource = 3
  toWord OpSourceExtension = 4
  toWord OpName = 5
  toWord OpMemberName = 6
  toWord OpString = 7
  toWord OpLine = 8
  toWord OpDecorate = 71
  toWord OpMemberDecorate = 72
  toWord OpDecorationGroup = 73
  toWord OpGroupDecorate = 74
  toWord OpGroupMemberDecorate = 75
  toWord OpExtension = 10
  toWord OpExtInstImport = 11
  toWord OpExtInst = 12
  toWord OpMemoryModel = 14
  toWord OpEntryPoint = 15
  toWord OpExecutionMode = 16
  toWord OpCapability = 17
  toWord OpTypeVoid = 19
  toWord OpTypeBool = 20
  toWord OpTypeInt = 21
  toWord OpTypeFloat = 22
  toWord OpTypeVector = 23
  toWord OpTypeMatrix = 24
  toWord OpTypeImage = 25
  toWord OpTypeSampler = 26
  toWord OpTypeSampledImage = 27
  toWord OpTypeArray = 28
  toWord OpTypeRuntimeArray = 29
  toWord OpTypeStruct = 30
  toWord OpTypeOpaque = 31
  toWord OpTypePointer = 32
  toWord OpTypeFunction = 33
  toWord OpTypeEvent = 34
  toWord OpTypeDeviceEvent = 35
  toWord OpTypeReserveId = 36
  toWord OpTypeQueue = 37
  toWord OpTypePipe = 38
  toWord OpConstantTrue = 41
  toWord OpConstantFalse = 42
  toWord OpConstant = 43
  toWord OpConstantComposite = 44
  toWord OpConstantSampler = 45
  toWord OpConstantNull = 46
  toWord OpSpecConstantTrue = 48
  toWord OpSpecConstantFalse = 49
  toWord OpSpecConstant = 50
  toWord OpSpecConstantComposite = 51
  toWord OpSpecConstantOp = 52
  toWord OpVariable = 59
  toWord OpImageTexelPointer = 60
  toWord OpLoad = 61
  toWord OpStore = 62
  toWord OpCopyMemory = 63
  toWord OpCopyMemorySized = 64
  toWord OpAccessChain = 65
  toWord OpInBoundsAccessChain = 66
  toWord OpPtrAccessChain = 67
  toWord OpArrayLength = 68
  toWord OpGenericPtrMemSemantics = 69
  toWord OpFunction = 54
  toWord OpFunctionParameter = 55
  toWord OpFunctionEnd = 56
  toWord OpFunctionCall = 57
  toWord OpSampledImage = 86
  toWord OpImageSampleImplicitLod = 87
  toWord OpImageSampleExplicitLod = 88
  toWord OpImageSampleDrefImplicitLod = 89
  toWord OpImageSampleDrefExplicitLod = 90
  toWord OpImageSampleProjImplicitLod = 91
  toWord OpImageSampleProjExplicitLod = 92
  toWord OpImageSampleProjDrefImplicitLod = 93
  toWord OpImageSampleProjDrefExplicitLod = 94
  toWord OpImageFetch = 95
  toWord OpImageGather = 96
  toWord OpImageDrefGather = 97
  toWord OpImageRead = 98
  toWord OpImageWrite = 99
  toWord OpImageQueryDim = 100
  toWord OpImageQueryFormat = 101
  toWord OpImageQueryOrder = 102
  toWord OpImageQuerySizeLod = 103
  toWord OpImageQuerySize = 104
  toWord OpImageQueryLod = 105
  toWord OpImageQueryLevels = 106
  toWord OpImageQuerySamples = 107
  toWord OpConvertFToU = 109
  toWord OpConvertFToS = 110
  toWord OpConvertSToF = 111
  toWord OpConvertUToF = 112
  toWord OpUConvert = 113
  toWord OpSConvert = 114
  toWord OpFConvert = 115
  toWord OpQuantizeToF16 = 116
  toWord OpConvertPtrToU = 117
  toWord OpSatConvertSToU = 118
  toWord OpSatConvertUToS = 119
  toWord OpConvertUToPtr = 120
  toWord OpPtrCastToGeneric = 121
  toWord OpGenericCastToPtr = 122
  toWord OpGenericCastToPtrExplicit = 123
  toWord OpBitcast = 124
  toWord OpVectorExtractDynamic = 77
  toWord OpVectorInsertDynamic = 78
  toWord OpVectorShuffle = 79
  toWord OpCompositeConstruct = 80
  toWord OpCompositeExtract = 81
  toWord OpCompositeInsert = 82
  toWord OpCopyObject = 83
  toWord OpTranspose = 84
  toWord OpSNegate = 126
  toWord OpFNegate = 127
  toWord OpIAdd = 128
  toWord OpFAdd = 129
  toWord OpISub = 130
  toWord OpFSub = 131
  toWord OpIMul = 132
  toWord OpFMul = 133
  toWord OpUDiv = 134
  toWord OpSDiv = 135
  toWord OpFDiv = 136
  toWord OpUMod = 137
  toWord OpSRem = 138
  toWord OpSMod = 139
  toWord OpFRem = 140
  toWord OpFMod = 141
  toWord OpVectorTimesScalar = 142
  toWord OpMatrixTimesScalar = 143
  toWord OpVectorTimesMatrix = 144
  toWord OpMatrixTimesVector = 145
  toWord OpMatrixTimesMatrix = 146
  toWord OpOuterProduct = 147
  toWord OpDot = 148
  toWord OpIAddCarry = 149
  toWord OpISubBorrow = 150
  toWord OpIMulExtended = 151
  toWord OpShiftRightLogical = 194
  toWord OpShiftRightArithmetic = 195
  toWord OpShiftLeftLogical = 196
  toWord OpBitwiseOr = 197
  toWord OpBitwiseXor = 198
  toWord OpBitwiseAnd = 199
  toWord OpNot = 200
  toWord OpBitFieldInsert = 201
  toWord OpBitFieldSExtract = 202
  toWord OpBitFieldUExtract = 203
  toWord OpBitReverse = 204
  toWord OpBitCount = 205
  toWord OpAny = 154
  toWord OpAll = 155
  toWord OpIsNan = 156
  toWord OpIsInf = 157
  toWord OpIsFinite = 158
  toWord OpIsNormal = 159
  toWord OpSignBitSet = 160
  toWord OpLessOrGreater = 161
  toWord OpOrdered = 162
  toWord OpUnordered = 163
  toWord OpLogicalEqual = 164
  toWord OpLogicalNotEqual = 165
  toWord OpLogicalOr = 166
  toWord OpLogicalAnd = 167
  toWord OpLogicalNot = 168
  toWord OpSelect = 169
  toWord OpIEqual = 170
  toWord OpINotEqual = 171
  toWord OpUGreaterThan = 172
  toWord OpSGreaterThan = 173
  toWord OpUGreaterThanEqual = 174
  toWord OpSGreaterThanEqual = 175
  toWord OpULessThan = 176
  toWord OpSLessThan = 177
  toWord OpULessThanEqual = 178
  toWord OpSLessThanEqual = 179
  toWord OpFOrdEqual = 180
  toWord OpFUnordEqual = 181
  toWord OpFOrdNotEqual = 182
  toWord OpFUnordNotEqual = 183
  toWord OpFOrdLessThan = 184
  toWord OpFUnordLessThan = 185
  toWord OpFOrdGreaterThan = 186
  toWord OpFUnordGreaterThan = 187
  toWord OpFOrdLessThanEqual = 188
  toWord OpFUnordLessThanEqual = 189
  toWord OpFOrdGreaterThanEqual = 190
  toWord OpFUnordGreaterThanEqual = 191
  toWord OpDPdx = 207
  toWord OpDPdy = 208
  toWord OpFwidth = 209
  toWord OpDPdxFine = 210
  toWord OpDPdyFine = 211
  toWord OpFwidthFine = 212
  toWord OpDPdxCoarse = 213
  toWord OpDPdyCoarse = 214
  toWord OpFwidthCoarse = 215
  toWord OpPhi = 245
  toWord OpLoopMerge = 246
  toWord OpSelectionMerge = 247
  toWord OpLabel = 248
  toWord OpBranch = 249
  toWord OpBranchConditional = 250
  toWord OpSwitch = 251
  toWord OpKill = 252
  toWord OpReturn = 253
  toWord OpReturnValue = 254
  toWord OpUnreachable = 255
  toWord OpLifetimeStart = 256
  toWord OpLifetimeStop = 257
  toWord OpAtomicLoad = 227
  toWord OpAtomicStore = 228
  toWord OpAtomicExchange = 229
  toWord OpAtomicCompareExchange = 230
  toWord OpAtomicCompareExchangeWeak = 231
  toWord OpAtomicIIncrement = 232
  toWord OpAtomicIDecrement = 233
  toWord OpAtomicIAdd = 234
  toWord OpAtomicISub = 235
  toWord OpAtomicSMin = 236
  toWord OpAtomicUMin = 237
  toWord OpAtomicSMax = 238
  toWord OpAtomicUMax = 239
  toWord OpAtomicAnd = 240
  toWord OpAtomicOr = 241
  toWord OpAtomicXor = 242
  toWord OpEmitVertex = 218
  toWord OpEndPrimitive = 219
  toWord OpEmitStreamVertex = 220
  toWord OpEndStreamPrimitive = 221
  toWord OpControlBarrier = 224
  toWord OpMemoryBarrier = 225
  toWord OpAsyncGroupCopy = 259
  toWord OpWaitGroupEvents = 260
  toWord OpGroupAll = 261
  toWord OpGroupAny = 262
  toWord OpGroupBroadcast = 263
  toWord OpGroupIAdd = 264
  toWord OpGroupFAdd = 265
  toWord OpGroupFMin = 266
  toWord OpGroupUMin = 267
  toWord OpGroupSMin = 268
  toWord OpGroupFMax = 269
  toWord OpGroupUMax = 270
  toWord OpGroupSMax = 271
  toWord OpEnqueueMarker = 291
  toWord OpEnqueueKernel = 292
  toWord OpGetKernelNDrangeSubGroupCount = 293
  toWord OpGetKernelNDrangeMaxSubGroupSize = 294
  toWord OpGetKernelWorkGroupSize = 295
  toWord OpGetKernelPreferredWorkGroupSizeMultiple = 296
  toWord OpRetainEvent = 297
  toWord OpReleaseEvent = 298
  toWord OpCreateUserEvent = 299
  toWord OpIsValidEvent = 300
  toWord OpSetUserEventStatus = 301
  toWord OpCaptureEventProfilingInfo = 302
  toWord OpGetDefaultQueue = 303
  toWord OpBuildNDRange = 304
  toWord OpReadPipe = 274
  toWord OpWritePipe = 275
  toWord OpReservedReadPipe = 276
  toWord OpReservedWritePipe = 277
  toWord OpReserveReadPipePackets = 278
  toWord OpReserveWritePipePackets = 279
  toWord OpCommitReadPipe = 280
  toWord OpCommitWritePipe = 281
  toWord OpIsValidReserveId = 282
  toWord OpGetNumPipePackets = 283
  toWord OpGetMaxPipePackets = 284
  toWord OpGroupReserveReadPipePackets = 285
  toWord OpGroupReserveWritePipePackets = 286
  toWord OpGroupCommitReadPipe = 287
  toWord OpGroupCommitWritePipe = 288

  fromWord 0 = Just OpNop
  fromWord 1 = Just OpUndef
  fromWord 3 = Just OpSource
  fromWord 4 = Just OpSourceExtension
  fromWord 5 = Just OpName
  fromWord 6 = Just OpMemberName
  fromWord 7 = Just OpString
  fromWord 8 = Just OpLine
  fromWord 71 = Just OpDecorate
  fromWord 72 = Just OpMemberDecorate
  fromWord 73 = Just OpDecorationGroup
  fromWord 74 = Just OpGroupDecorate
  fromWord 75 = Just OpGroupMemberDecorate
  fromWord 10 = Just OpExtension
  fromWord 11 = Just OpExtInstImport
  fromWord 12 = Just OpExtInst
  fromWord 14 = Just OpMemoryModel
  fromWord 15 = Just OpEntryPoint
  fromWord 16 = Just OpExecutionMode
  fromWord 17 = Just OpCapability
  fromWord 19 = Just OpTypeVoid
  fromWord 20 = Just OpTypeBool
  fromWord 21 = Just OpTypeInt
  fromWord 22 = Just OpTypeFloat
  fromWord 23 = Just OpTypeVector
  fromWord 24 = Just OpTypeMatrix
  fromWord 25 = Just OpTypeImage
  fromWord 26 = Just OpTypeSampler
  fromWord 27 = Just OpTypeSampledImage
  fromWord 28 = Just OpTypeArray
  fromWord 29 = Just OpTypeRuntimeArray
  fromWord 30 = Just OpTypeStruct
  fromWord 31 = Just OpTypeOpaque
  fromWord 32 = Just OpTypePointer
  fromWord 33 = Just OpTypeFunction
  fromWord 34 = Just OpTypeEvent
  fromWord 35 = Just OpTypeDeviceEvent
  fromWord 36 = Just OpTypeReserveId
  fromWord 37 = Just OpTypeQueue
  fromWord 38 = Just OpTypePipe
  fromWord 41 = Just OpConstantTrue
  fromWord 42 = Just OpConstantFalse
  fromWord 43 = Just OpConstant
  fromWord 44 = Just OpConstantComposite
  fromWord 45 = Just OpConstantSampler
  fromWord 46 = Just OpConstantNull
  fromWord 48 = Just OpSpecConstantTrue
  fromWord 49 = Just OpSpecConstantFalse
  fromWord 50 = Just OpSpecConstant
  fromWord 51 = Just OpSpecConstantComposite
  fromWord 52 = Just OpSpecConstantOp
  fromWord 59 = Just OpVariable
  fromWord 60 = Just OpImageTexelPointer
  fromWord 61 = Just OpLoad
  fromWord 62 = Just OpStore
  fromWord 63 = Just OpCopyMemory
  fromWord 64 = Just OpCopyMemorySized
  fromWord 65 = Just OpAccessChain
  fromWord 66 = Just OpInBoundsAccessChain
  fromWord 67 = Just OpPtrAccessChain
  fromWord 68 = Just OpArrayLength
  fromWord 69 = Just OpGenericPtrMemSemantics
  fromWord 54 = Just OpFunction
  fromWord 55 = Just OpFunctionParameter
  fromWord 56 = Just OpFunctionEnd
  fromWord 57 = Just OpFunctionCall
  fromWord 86 = Just OpSampledImage
  fromWord 87 = Just OpImageSampleImplicitLod
  fromWord 88 = Just OpImageSampleExplicitLod
  fromWord 89 = Just OpImageSampleDrefImplicitLod
  fromWord 90 = Just OpImageSampleDrefExplicitLod
  fromWord 91 = Just OpImageSampleProjImplicitLod
  fromWord 92 = Just OpImageSampleProjExplicitLod
  fromWord 93 = Just OpImageSampleProjDrefImplicitLod
  fromWord 94 = Just OpImageSampleProjDrefExplicitLod
  fromWord 95 = Just OpImageFetch
  fromWord 96 = Just OpImageGather
  fromWord 97 = Just OpImageDrefGather
  fromWord 98 = Just OpImageRead
  fromWord 99 = Just OpImageWrite
  fromWord 100 = Just OpImageQueryDim
  fromWord 101 = Just OpImageQueryFormat
  fromWord 102 = Just OpImageQueryOrder
  fromWord 103 = Just OpImageQuerySizeLod
  fromWord 104 = Just OpImageQuerySize
  fromWord 105 = Just OpImageQueryLod
  fromWord 106 = Just OpImageQueryLevels
  fromWord 107 = Just OpImageQuerySamples
  fromWord 109 = Just OpConvertFToU
  fromWord 110 = Just OpConvertFToS
  fromWord 111 = Just OpConvertSToF
  fromWord 112 = Just OpConvertUToF
  fromWord 113 = Just OpUConvert
  fromWord 114 = Just OpSConvert
  fromWord 115 = Just OpFConvert
  fromWord 116 = Just OpQuantizeToF16
  fromWord 117 = Just OpConvertPtrToU
  fromWord 118 = Just OpSatConvertSToU
  fromWord 119 = Just OpSatConvertUToS
  fromWord 120 = Just OpConvertUToPtr
  fromWord 121 = Just OpPtrCastToGeneric
  fromWord 122 = Just OpGenericCastToPtr
  fromWord 123 = Just OpGenericCastToPtrExplicit
  fromWord 124 = Just OpBitcast
  fromWord 77 = Just OpVectorExtractDynamic
  fromWord 78 = Just OpVectorInsertDynamic
  fromWord 79 = Just OpVectorShuffle
  fromWord 80 = Just OpCompositeConstruct
  fromWord 81 = Just OpCompositeExtract
  fromWord 82 = Just OpCompositeInsert
  fromWord 83 = Just OpCopyObject
  fromWord 84 = Just OpTranspose
  fromWord 126 = Just OpSNegate
  fromWord 127 = Just OpFNegate
  fromWord 128 = Just OpIAdd
  fromWord 129 = Just OpFAdd
  fromWord 130 = Just OpISub
  fromWord 131 = Just OpFSub
  fromWord 132 = Just OpIMul
  fromWord 133 = Just OpFMul
  fromWord 134 = Just OpUDiv
  fromWord 135 = Just OpSDiv
  fromWord 136 = Just OpFDiv
  fromWord 137 = Just OpUMod
  fromWord 138 = Just OpSRem
  fromWord 139 = Just OpSMod
  fromWord 140 = Just OpFRem
  fromWord 141 = Just OpFMod
  fromWord 142 = Just OpVectorTimesScalar
  fromWord 143 = Just OpMatrixTimesScalar
  fromWord 144 = Just OpVectorTimesMatrix
  fromWord 145 = Just OpMatrixTimesVector
  fromWord 146 = Just OpMatrixTimesMatrix
  fromWord 147 = Just OpOuterProduct
  fromWord 148 = Just OpDot
  fromWord 149 = Just OpIAddCarry
  fromWord 150 = Just OpISubBorrow
  fromWord 151 = Just OpIMulExtended
  fromWord 194 = Just OpShiftRightLogical
  fromWord 195 = Just OpShiftRightArithmetic
  fromWord 196 = Just OpShiftLeftLogical
  fromWord 197 = Just OpBitwiseOr
  fromWord 198 = Just OpBitwiseXor
  fromWord 199 = Just OpBitwiseAnd
  fromWord 200 = Just OpNot
  fromWord 201 = Just OpBitFieldInsert
  fromWord 202 = Just OpBitFieldSExtract
  fromWord 203 = Just OpBitFieldUExtract
  fromWord 204 = Just OpBitReverse
  fromWord 205 = Just OpBitCount
  fromWord 154 = Just OpAny
  fromWord 155 = Just OpAll
  fromWord 156 = Just OpIsNan
  fromWord 157 = Just OpIsInf
  fromWord 158 = Just OpIsFinite
  fromWord 159 = Just OpIsNormal
  fromWord 160 = Just OpSignBitSet
  fromWord 161 = Just OpLessOrGreater
  fromWord 162 = Just OpOrdered
  fromWord 163 = Just OpUnordered
  fromWord 164 = Just OpLogicalEqual
  fromWord 165 = Just OpLogicalNotEqual
  fromWord 166 = Just OpLogicalOr
  fromWord 167 = Just OpLogicalAnd
  fromWord 168 = Just OpLogicalNot
  fromWord 169 = Just OpSelect
  fromWord 170 = Just OpIEqual
  fromWord 171 = Just OpINotEqual
  fromWord 172 = Just OpUGreaterThan
  fromWord 173 = Just OpSGreaterThan
  fromWord 174 = Just OpUGreaterThanEqual
  fromWord 175 = Just OpSGreaterThanEqual
  fromWord 176 = Just OpULessThan
  fromWord 177 = Just OpSLessThan
  fromWord 178 = Just OpULessThanEqual
  fromWord 179 = Just OpSLessThanEqual
  fromWord 180 = Just OpFOrdEqual
  fromWord 181 = Just OpFUnordEqual
  fromWord 182 = Just OpFOrdNotEqual
  fromWord 183 = Just OpFUnordNotEqual
  fromWord 184 = Just OpFOrdLessThan
  fromWord 185 = Just OpFUnordLessThan
  fromWord 186 = Just OpFOrdGreaterThan
  fromWord 187 = Just OpFUnordGreaterThan
  fromWord 188 = Just OpFOrdLessThanEqual
  fromWord 189 = Just OpFUnordLessThanEqual
  fromWord 190 = Just OpFOrdGreaterThanEqual
  fromWord 191 = Just OpFUnordGreaterThanEqual
  fromWord 207 = Just OpDPdx
  fromWord 208 = Just OpDPdy
  fromWord 209 = Just OpFwidth
  fromWord 210 = Just OpDPdxFine
  fromWord 211 = Just OpDPdyFine
  fromWord 212 = Just OpFwidthFine
  fromWord 213 = Just OpDPdxCoarse
  fromWord 214 = Just OpDPdyCoarse
  fromWord 215 = Just OpFwidthCoarse
  fromWord 245 = Just OpPhi
  fromWord 246 = Just OpLoopMerge
  fromWord 247 = Just OpSelectionMerge
  fromWord 248 = Just OpLabel
  fromWord 249 = Just OpBranch
  fromWord 250 = Just OpBranchConditional
  fromWord 251 = Just OpSwitch
  fromWord 252 = Just OpKill
  fromWord 253 = Just OpReturn
  fromWord 254 = Just OpReturnValue
  fromWord 255 = Just OpUnreachable
  fromWord 256 = Just OpLifetimeStart
  fromWord 257 = Just OpLifetimeStop
  fromWord 227 = Just OpAtomicLoad
  fromWord 228 = Just OpAtomicStore
  fromWord 229 = Just OpAtomicExchange
  fromWord 230 = Just OpAtomicCompareExchange
  fromWord 231 = Just OpAtomicCompareExchangeWeak
  fromWord 232 = Just OpAtomicIIncrement
  fromWord 233 = Just OpAtomicIDecrement
  fromWord 234 = Just OpAtomicIAdd
  fromWord 235 = Just OpAtomicISub
  fromWord 236 = Just OpAtomicSMin
  fromWord 237 = Just OpAtomicUMin
  fromWord 238 = Just OpAtomicSMax
  fromWord 239 = Just OpAtomicUMax
  fromWord 240 = Just OpAtomicAnd
  fromWord 241 = Just OpAtomicOr
  fromWord 242 = Just OpAtomicXor
  fromWord 218 = Just OpEmitVertex
  fromWord 219 = Just OpEndPrimitive
  fromWord 220 = Just OpEmitStreamVertex
  fromWord 221 = Just OpEndStreamPrimitive
  fromWord 224 = Just OpControlBarrier
  fromWord 225 = Just OpMemoryBarrier
  fromWord 259 = Just OpAsyncGroupCopy
  fromWord 260 = Just OpWaitGroupEvents
  fromWord 261 = Just OpGroupAll
  fromWord 262 = Just OpGroupAny
  fromWord 263 = Just OpGroupBroadcast
  fromWord 264 = Just OpGroupIAdd
  fromWord 265 = Just OpGroupFAdd
  fromWord 266 = Just OpGroupFMin
  fromWord 267 = Just OpGroupUMin
  fromWord 268 = Just OpGroupSMin
  fromWord 269 = Just OpGroupFMax
  fromWord 270 = Just OpGroupUMax
  fromWord 271 = Just OpGroupSMax
  fromWord 291 = Just OpEnqueueMarker
  fromWord 292 = Just OpEnqueueKernel
  fromWord 293 = Just OpGetKernelNDrangeSubGroupCount
  fromWord 294 = Just OpGetKernelNDrangeMaxSubGroupSize
  fromWord 295 = Just OpGetKernelWorkGroupSize
  fromWord 296 = Just OpGetKernelPreferredWorkGroupSizeMultiple
  fromWord 297 = Just OpRetainEvent
  fromWord 298 = Just OpReleaseEvent
  fromWord 299 = Just OpCreateUserEvent
  fromWord 300 = Just OpIsValidEvent
  fromWord 301 = Just OpSetUserEventStatus
  fromWord 302 = Just OpCaptureEventProfilingInfo
  fromWord 303 = Just OpGetDefaultQueue
  fromWord 304 = Just OpBuildNDRange
  fromWord 274 = Just OpReadPipe
  fromWord 275 = Just OpWritePipe
  fromWord 276 = Just OpReservedReadPipe
  fromWord 277 = Just OpReservedWritePipe
  fromWord 278 = Just OpReserveReadPipePackets
  fromWord 279 = Just OpReserveWritePipePackets
  fromWord 280 = Just OpCommitReadPipe
  fromWord 281 = Just OpCommitWritePipe
  fromWord 282 = Just OpIsValidReserveId
  fromWord 283 = Just OpGetNumPipePackets
  fromWord 284 = Just OpGetMaxPipePackets
  fromWord 285 = Just OpGroupReserveReadPipePackets
  fromWord 286 = Just OpGroupReserveWritePipePackets
  fromWord 287 = Just OpGroupCommitReadPipe
  fromWord 288 = Just OpGroupCommitWritePipe
  fromWord _ = Nothing

  requiredCapabilities OpTypeMatrix = [Capability.Matrix]
  requiredCapabilities OpTypeRuntimeArray = [Capability.Shader]
  requiredCapabilities OpTypeOpaque = [Capability.Kernel]
  requiredCapabilities OpTypeEvent = [Capability.Kernel]
  requiredCapabilities OpTypeDeviceEvent = [Capability.Kernel]
  requiredCapabilities OpTypeReserveId = [Capability.Pipes]
  requiredCapabilities OpTypeQueue = [Capability.Kernel]
  requiredCapabilities OpTypePipe = [Capability.Pipes]
  requiredCapabilities OpConstantSampler = [Capability.LiteralSampler]
  requiredCapabilities OpCopyMemorySized = [Capability.Addresses]
  requiredCapabilities OpPtrAccessChain = [Capability.Addresses]
  requiredCapabilities OpArrayLength = [Capability.Shader]
  requiredCapabilities OpGenericPtrMemSemantics = [Capability.Kernel]
  requiredCapabilities OpImageSampleImplicitLod = [Capability.Shader]
  requiredCapabilities OpImageSampleDrefImplicitLod = [Capability.Shader]
  requiredCapabilities OpImageSampleDrefExplicitLod = [Capability.Shader]
  requiredCapabilities OpImageSampleProjImplicitLod = [Capability.Shader]
  requiredCapabilities OpImageSampleProjExplicitLod = [Capability.Shader]
  requiredCapabilities OpImageSampleProjDrefImplicitLod = [Capability.Shader]
  requiredCapabilities OpImageSampleProjDrefExplicitLod = [Capability.Shader]
  requiredCapabilities OpImageGather = [Capability.Shader]
  requiredCapabilities OpImageDrefGather = [Capability.Shader]
  requiredCapabilities OpImageQueryDim = [Capability.Kernel]
  requiredCapabilities OpImageQueryFormat = [Capability.Kernel]
  requiredCapabilities OpImageQueryOrder = [Capability.Kernel]
  requiredCapabilities OpImageQueryLod = [Capability.Shader]
  requiredCapabilities OpImageQueryLevels = [Capability.Shader]
  requiredCapabilities OpImageQuerySamples = [Capability.Shader]
  requiredCapabilities OpQuantizeToF16 = [Capability.Shader]
  requiredCapabilities OpConvertPtrToU = [Capability.Addresses]
  requiredCapabilities OpSatConvertSToU = [Capability.Kernel]
  requiredCapabilities OpSatConvertUToS = [Capability.Kernel]
  requiredCapabilities OpConvertUToPtr = [Capability.Addresses]
  requiredCapabilities OpPtrCastToGeneric = [Capability.Kernel]
  requiredCapabilities OpGenericCastToPtr = [Capability.Kernel]
  requiredCapabilities OpGenericCastToPtrExplicit = [Capability.Kernel]
  requiredCapabilities OpTranspose = [Capability.Matrix]
  requiredCapabilities OpMatrixTimesScalar = [Capability.Matrix]
  requiredCapabilities OpVectorTimesMatrix = [Capability.Matrix]
  requiredCapabilities OpMatrixTimesVector = [Capability.Matrix]
  requiredCapabilities OpMatrixTimesMatrix = [Capability.Matrix]
  requiredCapabilities OpOuterProduct = [Capability.Matrix]
  requiredCapabilities OpBitFieldInsert = [Capability.Shader]
  requiredCapabilities OpBitFieldSExtract = [Capability.Shader]
  requiredCapabilities OpBitFieldUExtract = [Capability.Shader]
  requiredCapabilities OpBitReverse = [Capability.Shader]
  requiredCapabilities OpIsFinite = [Capability.Kernel]
  requiredCapabilities OpIsNormal = [Capability.Kernel]
  requiredCapabilities OpSignBitSet = [Capability.Kernel]
  requiredCapabilities OpLessOrGreater = [Capability.Kernel]
  requiredCapabilities OpOrdered = [Capability.Kernel]
  requiredCapabilities OpUnordered = [Capability.Kernel]
  requiredCapabilities OpDPdx = [Capability.Shader]
  requiredCapabilities OpDPdy = [Capability.Shader]
  requiredCapabilities OpFwidth = [Capability.Shader]
  requiredCapabilities OpDPdxFine = [Capability.Shader]
  requiredCapabilities OpDPdyFine = [Capability.Shader]
  requiredCapabilities OpFwidthFine = [Capability.Shader]
  requiredCapabilities OpDPdxCoarse = [Capability.Shader]
  requiredCapabilities OpDPdyCoarse = [Capability.Shader]
  requiredCapabilities OpFwidthCoarse = [Capability.Shader]
  requiredCapabilities OpKill = [Capability.Shader]
  requiredCapabilities OpAtomicCompareExchangeWeak = [Capability.Kernel]
  requiredCapabilities OpEmitVertex = [Capability.Geometry]
  requiredCapabilities OpEndPrimitive = [Capability.Geometry]
  requiredCapabilities OpEmitStreamVertex = [Capability.Geometry]
  requiredCapabilities OpEndStreamPrimitive = [Capability.Geometry]
  requiredCapabilities OpAsyncGroupCopy = [Capability.Kernel]
  requiredCapabilities OpWaitGroupEvents = [Capability.Kernel]
  requiredCapabilities OpGroupAll = [Capability.Groups]
  requiredCapabilities OpGroupAny = [Capability.Groups]
  requiredCapabilities OpGroupBroadcast = [Capability.Groups]
  requiredCapabilities OpGroupIAdd = [Capability.Groups]
  requiredCapabilities OpGroupFAdd = [Capability.Groups]
  requiredCapabilities OpGroupFMin = [Capability.Groups]
  requiredCapabilities OpGroupUMin = [Capability.Groups]
  requiredCapabilities OpGroupSMin = [Capability.Groups]
  requiredCapabilities OpGroupFMax = [Capability.Groups]
  requiredCapabilities OpGroupUMax = [Capability.Groups]
  requiredCapabilities OpGroupSMax = [Capability.Groups]
  requiredCapabilities OpEnqueueMarker = [Capability.DeviceEnqueue]
  requiredCapabilities OpEnqueueKernel = [Capability.DeviceEnqueue]
  requiredCapabilities OpGetKernelNDrangeSubGroupCount = [Capability.DeviceEnqueue]
  requiredCapabilities OpGetKernelNDrangeMaxSubGroupSize = [Capability.DeviceEnqueue]
  requiredCapabilities OpGetKernelWorkGroupSize = [Capability.DeviceEnqueue]
  requiredCapabilities OpGetKernelPreferredWorkGroupSizeMultiple = [Capability.DeviceEnqueue]
  requiredCapabilities OpRetainEvent = [Capability.DeviceEnqueue]
  requiredCapabilities OpReleaseEvent = [Capability.DeviceEnqueue]
  requiredCapabilities OpCreateUserEvent = [Capability.DeviceEnqueue]
  requiredCapabilities OpIsValidEvent = [Capability.DeviceEnqueue]
  requiredCapabilities OpSetUserEventStatus = [Capability.DeviceEnqueue]
  requiredCapabilities OpCaptureEventProfilingInfo = [Capability.DeviceEnqueue]
  requiredCapabilities OpGetDefaultQueue = [Capability.DeviceEnqueue]
  requiredCapabilities OpBuildNDRange = [Capability.DeviceEnqueue]
  requiredCapabilities OpReadPipe = [Capability.Pipes]
  requiredCapabilities OpWritePipe = [Capability.Pipes]
  requiredCapabilities OpReservedReadPipe = [Capability.Pipes]
  requiredCapabilities OpReservedWritePipe = [Capability.Pipes]
  requiredCapabilities OpReserveReadPipePackets = [Capability.Pipes]
  requiredCapabilities OpReserveWritePipePackets = [Capability.Pipes]
  requiredCapabilities OpCommitReadPipe = [Capability.Pipes]
  requiredCapabilities OpCommitWritePipe = [Capability.Pipes]
  requiredCapabilities OpIsValidReserveId = [Capability.Pipes]
  requiredCapabilities OpGetNumPipePackets = [Capability.Pipes]
  requiredCapabilities OpGetMaxPipePackets = [Capability.Pipes]
  requiredCapabilities OpGroupReserveReadPipePackets = [Capability.Pipes]
  requiredCapabilities OpGroupReserveWritePipePackets = [Capability.Pipes]
  requiredCapabilities OpGroupCommitReadPipe = [Capability.Pipes]
  requiredCapabilities OpGroupCommitWritePipe = [Capability.Pipes]
  requiredCapabilities _ = []
