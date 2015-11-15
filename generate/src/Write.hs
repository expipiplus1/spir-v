{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Write
  ( writeSpecToFiles
  ) where

import Control.Monad ((<=<))
import Data.Char (isAlphaNum, isDigit, toUpper)
import Data.Foldable (foldl')
import Data.List (isPrefixOf, intersperse)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Word (Word32)
import Numeric (showHex)
import Safe (readMay)
import Spec
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import Text.InterpolatedString.Perl6 (qc)
import Text.PrettyPrint.Leijen.Text (Doc, vsep, hsep, line, hang, indent, align, empty, (<+>), (<$$>))

writeSpecToFiles :: FilePath -> Spec -> IO ()
writeSpecToFiles d Spec{..} =
  do let createParents = True
     createDirectoryIfMissing createParents (d </> "Language" </> "SpirV")
     writeLimitsFile d specLimits
     writeMagicFile d specMagic
     mapM_ (writeSubsectionFile d) specStandardSubsections
     writeInstructionGroups d specInstructionGroups
     pure ()

--------------------------------------------------------------------------------
-- Limits
--------------------------------------------------------------------------------

writeLimitsFile :: FilePath -> [Limit] -> IO ()
writeLimitsFile d ls = writeFile (d </> "Language" </> "SpirV" </> "Limits.hs") limitModule
  where limitModule = [qc|module Language.SpirV.Limits where

import Data.Word (Word32)
{vsep $ limitDefinition <$> ls}
|]
        limitDefinition :: Limit -> Doc
        limitDefinition Limit{..} =
          let n = limitName limitEntity
          in [qc|
max{n} :: Word32
max{n} = {limitUpperLimit}|]

limitName :: String -> String
limitName n = if "Indexes" `isPrefixOf` n
                then "Index"
                else pascalCase . removeParens $ n

--------------------------------------------------------------------------------
-- Magic
--------------------------------------------------------------------------------

writeMagicFile :: FilePath -> Word32 -> IO ()
writeMagicFile d m = writeFile (d </> "Language" </> "SpirV" </> "Magic.hs") magicModule
  where magicModule = [qc|module Language.SpirV.Magic where

import Data.Word (Word32)

magic :: Word32
magic = 0x{showHex m ""}
|]


--------------------------------------------------------------------------------
-- The standard subsections
-- Because some names overlap, these each have their own file
--------------------------------------------------------------------------------

writeSubsectionFile :: FilePath -> StandardSubsection -> IO ()
writeSubsectionFile d sss@StandardSubsection{..} =
  writeFile (d </> "Language" </> "SpirV" </> moduleName <.> "hs") subsectionModule
  where moduleName = pascalCase sssName

        isCapabilityModule = moduleName == "Capability"

        enumConstructors =
          let (e:es) = (fromString . pascalCase . eeName) <$> sssEnumTable
              cs = indent 2 e : (("|" <+>) <$> es)
          in zipWith (\enum c -> indent 2 (enumConstructorDocumentation enum) <> line <> c) sssEnumTable cs

        makeToWordDecl EnumElement{..} =
          [qc|toWord {pascalCase eeName} = {eeEnum}|]

        makeFromWordDecl EnumElement{..} =
          [qc|fromWord {eeEnum} = Just {pascalCase eeName}|]

        makeRequiredCapabilityDecl EnumElement{..} =
          case eeRequiredCapability of
            Nothing -> ""
            Just s -> [qc|requiredCapabilities {pascalCase eeName} = [{if isCapabilityModule then empty else "Capability."}{pascalCase s}]|]

        subsectionModule = [qc|\{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.{moduleName} where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
{if isCapabilityModule then "" else "import qualified Language.SpirV.Capability as Capability" <> line}

{subsectionDocumentation sss}
data {moduleName} =
{indent 2 . vsep $ enumConstructors}
  deriving(Read, Show, Eq, Ord)

instance SpirEnum {moduleName} Word32 where
  {hang 2 . vsep $ fmap makeToWordDecl sssEnumTable}

  {hang 2 . vsep $ fmap makeFromWordDecl sssEnumTable}
  fromWord _ = Nothing

  {hang 2 . vsep $ fmap makeRequiredCapabilityDecl sssEnumTable}
  {if any (== Nothing) (eeRequiredCapability <$> sssEnumTable) then "requiredCapabilities _ = []" else empty}
|]

subsectionDocumentation :: StandardSubsection -> Doc
subsectionDocumentation StandardSubsection{..} = docLineComents commentLines
  where commentLines = (fromString <$> lines sssDescription) ++ [sourceLink sssId sssName]

-- | enumConstructorDocumentation returns a sensible bit of documentation from
-- the description of the enumeration
enumConstructorDocumentation :: EnumElement -> Doc
enumConstructorDocumentation EnumElement{..} = docLineComents (fromString <$> lines eeDescription)

docLineComents :: [Doc] -> Doc
docLineComents cs = let isEmpty d = show d == ""
                    in case filter (not . isEmpty) cs of
                         [] -> empty
                         (l:ls) -> vsep $ intersperse "--" (("-- |" <+> l) : (("--" <+>) <$> ls))

--------------------------------------------------------------------------------
-- Instruction Groups
--------------------------------------------------------------------------------

writeInstructionGroups :: FilePath -> [InstructionGroup] -> IO ()
writeInstructionGroups d gs = writeFile
                              (d </> "Language" </> "SpirV" </> "OpCode.hs")
                              instructionModule
  where instructions = gs >>= (\(InstructionGroup _ _ is) -> is)

        instructionConstructors =
          let (i:is) = (fromString . pascalCase . instructionName) <$> instructions
              cs = indent 2 i : (("|" <+>) <$> is)
          in zipWith (\ins c -> indent 2 (opCodeConstructorDocumentation ins) <> line <> c) instructions cs

        makeToWordDecl Instruction{..} =
          [qc|toWord {pascalCase instructionName} = {instructionOpCode}|]

        makeFromWordDecl Instruction{..} =
          [qc|fromWord {instructionOpCode} = Just {pascalCase instructionName}|]

        makeRequiredCapabilityDecl Instruction{..} =
          case instructionRequiredCapabilities of
            [] -> ""
            cs -> [qc|requiredCapabilities {pascalCase instructionName} = [{hsep $ intersperse ", " ((("Capability." <>) . fromString . pascalCase) <$> cs)}]|]

        makeInstructionSizeDecl Instruction{..} =
          [qc|instructionSize {pascalCase instructionName} = {if instructionVariadicOperand == Nothing then "Fixed" else "Variadic" :: Doc} {instructionWordCount}|]

        instructionModule = [qc|\{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.OpCode where

import           Data.Word (Word16)
import qualified Language.SpirV.Capability as Capability
import           Language.SpirV.SpirEnum

data OpCode =
{indent 2 . vsep $ instructionConstructors}
  deriving(Read, Show, Eq, Ord)

data InstructionSize = Fixed Word16
                     | Variadic Word16
  deriving(Read, Show, Eq)

instructionSize :: OpCode -> InstructionSize
{vsep $ fmap makeInstructionSizeDecl instructions}

instance SpirEnum OpCode Word16 where
{indent 2 . vsep $ fmap makeToWordDecl instructions}

{indent 2 . vsep $ fmap makeFromWordDecl instructions}
  fromWord _ = Nothing

{indent 2 . vsep $ fmap makeRequiredCapabilityDecl instructions}
  {if any (== []) (instructionRequiredCapabilities <$> instructions) then "requiredCapabilities _ = []" else empty}
|]

-- | opCodeConstructorDocumentation returns a sensible bit of documentation from
-- the description of the instruction
opCodeConstructorDocumentation :: Instruction -> Doc
opCodeConstructorDocumentation Instruction{..} =
  align $ ("-- |" <+> l) <$$>
  "--" <$$>
  ("--" <+> sourceLink instructionName instructionName)
  where (l:_) = fromString <$> lines instructionDescription

sourceLink :: String -> String -> Doc
sourceLink identifier name =
  "<" <> "https:\\/\\/www.khronos.org\\/registry\\/spir-v\\/specs\\/1.0\\/SPIRV.html" <>
  "#" <> fromString identifier <+> "Source for" <+> fromString name <> ">"

--------------------------------------------------------------------------------
-- Some utilities
--------------------------------------------------------------------------------

removeParens :: String -> String
removeParens = reverse . snd . foldl' go (0::Int, [])
  where go (n, acc) '(' = (n+1, acc)
        go (n, acc) ')' = (max 0 (n-1), acc)
        go (0, acc) x = (0, x : acc)
        go (n, acc) _ = (n, acc)

pascalCase :: String -> String
pascalCase [] = error "pascalCaseCase given empty string"
pascalCase s = let (l:_) = lines s
                   (w:ws) = filter (not . null) . fmap (filter isAlphaNum) $ words l
                   ws' = upperFirst <$> (correctFirstWord w ++ ws)
                   upperFirst "" = ""
                   upperFirst (x:xs) = toUpper x : xs
                   correctFirstWord n = let (is, ns) = span isDigit n
                                        in fromJust (traverse (digitCardinal <=< readMay . pure) is) ++ [ns]
               in concat ws'

{-
camelCase :: String -> String
camelCase s = let (x:xs) = pascalCase s
                 -- it might be sensible to toLower a string of uppercase
                 -- letters at the start
              in toLower x : xs
              -}

digitCardinal :: Int -> Maybe String
digitCardinal = \case
  0 -> Just "zero"
  1 -> Just "one"
  2 -> Just "two"
  3 -> Just "three"
  4 -> Just "four"
  5 -> Just "five"
  6 -> Just "six"
  7 -> Just "seven"
  8 -> Just "eight"
  9 -> Just "nine"
  _ -> Nothing


