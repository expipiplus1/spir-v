module Language.SpirV.Limits where

import Data.Word (Word32)

maxCharactersInALiteralString :: Word32
maxCharactersInALiteralString = 65535

maxInstructionWordCount :: Word32
maxInstructionWordCount = 65535

maxResultIdBound :: Word32
maxResultIdBound = 4194303

maxControlflowNestingDepth :: Word32
maxControlflowNestingDepth = 1023

maxGlobalVariables :: Word32
maxGlobalVariables = 65535

maxLocalVariables :: Word32
maxLocalVariables = 524287

maxExecutionModesPerEntryPoint :: Word32
maxExecutionModesPerEntryPoint = 255

maxIndex :: Word32
maxIndex = 255

maxNumberOfFunctionParametersPerFunctionDeclaration :: Word32
maxNumberOfFunctionParametersPerFunctionDeclaration = 255

maxOpFunctionCallActualArguments :: Word32
maxOpFunctionCallActualArguments = 255

maxOpExtInstActualArguments :: Word32
maxOpExtInstActualArguments = 255

maxOpSwitchPairs :: Word32
maxOpSwitchPairs = 16383

maxOpTypeStructMembers :: Word32
maxOpTypeStructMembers = 16383

maxStructureNestingDepth :: Word32
maxStructureNestingDepth = 255
