module Spec
  ( Spec(..)
  , Limit(..)
  , StandardSubsection(..)
  , EnumElement(..)
  , Operand(..)
  , SubsectionNumber
  , SubsubsectionNumber
  , InstructionGroup(..)
  , Instruction(..)
  , VariadicOperand(..)
  ) where

import Data.Version (Version)
import Data.Word (Word32, Word16)

data Spec = Spec { specVersion :: Version
                 --, specCapabilities :: [Capability]
                 , specLimits :: [Limit]
                 , specMagic :: Word32
                 , specStandardSubsections :: [StandardSubsection]
                 , specInstructionGroups :: [InstructionGroup]
                 }
  deriving Show

{-
data Capability = Capability { capabilityAbbreviation :: String
                             , capabilityDescription :: String
                             , capabilityDependency :: Maybe String
                             }
                             -}

data Limit = Limit { limitEntity :: String
                   , limitUpperLimit :: Integer
                   }
  deriving Show

{-|
  Lots of subsections in the specification are of the form:

  1.2 Section Name

  Description

  -------------------------------------------------------------------
  | Name                 | Required Capability     | Extra Operands |
  |----------------------|-------------------------|----------------|
  | 0 | element name     | capability abbreviation | type           |
  |   | elem description |                         | name           |
  |---|------------------|-------------------------|----------------|
  | 1 | element name     | capability abbreviation | type  | type   |
  |   | elem description |                         | name  | name   |
  ...

  "Description" is optional
  "elem description" is optional
  The Columns "Required Capability" and "Extra Operands" are optional
  elements in those columns are also optional

  All of these sections are parsed into a StandardSubsection
-}
data StandardSubsection =
  StandardSubsection { sssSubsectionNumber :: SubsectionNumber
                     , sssName :: String
                     , sssId :: String
                     , sssDescription :: String
                     , sssEnumTable :: [EnumElement]
                     }
  deriving Show

type SubsectionNumber = (Int, Int)
type SubsubsectionNumber = (Int, Int, Int)

-- | An EnumElement represents a single row in a standard subsection table,
data EnumElement = EnumElement { eeEnum :: Word32
                               , eeName :: String
                               , eeDescription :: String
                               , eeRequiredCapability :: Maybe String
                               , eeExtraOperands :: [Operand]
                               }
  deriving Show

-- | An Operand represents a value in the "Extra Operands" column of a standard subsection table
data Operand = Operand { operandType :: String
                       , operandName :: String
                       }
  deriving (Show, Eq)

-- | Instructions are grouped according to which subsubsection they appear in
data InstructionGroup = InstructionGroup SubsubsectionNumber String [Instruction]
  deriving Show

-- | Instructions as parsed from instruction tables
data Instruction = Instruction { -- | The instruction name, "OpName"
                                 instructionName :: String
                                 -- | The description of the instruction and
                                 -- its parameters
                               , instructionDescription :: String
                                 -- | Any required capabilities
                               , instructionRequiredCapabilities :: [String]
                                 -- | For a variadic instruction this is the
                                 -- minimum size of the instruction For a fixed
                                 -- size instruction this is the number of
                                 -- words it uses up
                               , instructionWordCount :: Word16
                                 -- | The opcode of the instruction
                               , instructionOpCode :: Word16
                                 -- | A list of all the non-variadic operands.
                                 -- This will include the
                               , instructionFixedOperands :: [Operand]
                                 -- | The variadic operand representation if
                                 -- this is a variadic instruction
                               , instructionVariadicOperand :: Maybe VariadicOperand

                               }
  deriving Show

data VariadicOperand = -- | This is a string operand
                       VariadicOperandString Operand
                       -- | This is a repeated operand, all with the same type
                     | VariadicOperandRepeated Operand
                       -- | This is an optional operand
                     | VariadicOperandOptional Operand
  deriving (Show, Eq)

