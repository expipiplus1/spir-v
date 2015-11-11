{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parse
  ( parseSpec
  ) where

import Control.Applicative ((<|>))
import Control.Arrow.ArrowList.Extra (arrF)
import Control.Monad (guard, (<=<))
import Data.Attoparsec.Text (Parser, parseOnly, hexadecimal, skipSpace, takeTill, anyChar, takeText, decimal, isEndOfLine, many1, sepBy1, string, satisfy)
import Data.Char (isSpace, isDigit)
import Data.Foldable (minimumBy, asum)
import Data.Function (on)
import Data.List (isInfixOf)
import Data.Maybe.Extra (rightToMaybe, fromMaybe, catMaybes, listToMaybe)
import Data.Text (pack, unpack)
import Data.Version (Version, parseVersion)
import Data.Word  (Word32, Word16)
import Safe (headMay, atMay, tailMay, initMay, lastMay)
import Text.Read (readMaybe)
import Text.XML.HXT.Core hiding (xshow, trace)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)

import ConvertHTML
import Spec
import Table

parseSpec :: String -> IO (Maybe Spec)
parseSpec t =
  let doc = readString [withParseHTML yes, withWarnings yes] t
  in headMay <$> runX (doc >>> parseSpecHTML)

parseSpecHTML :: IOSLA (XIOState ()) XmlTree Spec
parseSpecHTML =
  proc doc ->
  do content <- deep (hasAttrValue "id" (=="content")) -< doc
     version <- getVersion -< doc
     limits <- deep getLimits -< content
     magic <- deep getMagic -< content
     standardSubSections <- listA (deep getStandardSubsection) -< content
     instructionGroups <- listA (deep getInstructionGroup <<< deep isInstructionSubsection) -< content
     returnA -< Spec { specVersion = version
                     , specLimits = limits
                     , specMagic = magic
                     , specStandardSubsections = standardSubSections
                     , specInstructionGroups = instructionGroups
                     }

--------------------------------------------------------------------------------
-- Parsing the header
--------------------------------------------------------------------------------

getVersion :: ArrowXml a => a XmlTree Version
getVersion = parseHeader

parseHeader :: ArrowXml a => a XmlTree Version
parseHeader =
  deep (hasAttrValue "id" (== "revnumber") //> getText >>^ parseVersionText)

-- | parseVersionText takes a string like "words words 1.2.3 words" and
-- extracts the version
parseVersionText :: String -> Version
parseVersionText = fromMaybe (error "Unable to parse version") .
                   parseInWords parseVersion

--------------------------------------------------------------------------------
-- Limits
--------------------------------------------------------------------------------

getLimits :: ArrowXml a => a XmlTree [Limit]
getLimits =
      hasSubsectionId "_a_id_limits_a_universal_limits"
  >>> single (deep getHeadlessTable)
  >>> arrF (traverse htmlToPlain)
  >>^ limitsFromTable
  where limitsFromTable (Table _ body) =
          -- use catMaybes because this table is oddly formed, and some rows
          -- should be ignored
          catMaybes . fmap limitFromRow $ body
        limitFromRow [entity, dec, _] = Limit entity <$> parseMarkedDecimal dec
        limitFromRow _ = Nothing

--------------------------------------------------------------------------------
-- Magic
--------------------------------------------------------------------------------

getMagic :: ArrowXml a => a XmlTree Word32
getMagic = getTable >>> arrF (traverse htmlToPlain) >>> arrF magicFromTable
  where -- magicFromTable :: Table -> Either String Word32
        magicFromTable (Table ["Magic Number"] [[n]]) =
          parseOnly ("0x" *> hexadecimal) (pack n)
        magicFromTable _ = Left ""

--------------------------------------------------------------------------------
-- Standard Subsections
--------------------------------------------------------------------------------

getStandardSubsection :: ArrowXml a => a XmlTree StandardSubsection
getStandardSubsection =
  proc tree -> do
    subsection  <- isSubsection -< tree
    title       <- single (hasName "h3" <<< getChildren) -< subsection
    titleText   <- arrF htmlToPlain -< title
    titleId     <- getAttrValue "id" -< title
    -- TODO: html descriptions
    description <- concat ^<< listA (arrF htmlToHaddock <<< neg isTable <<< neg (hasName "h3") <<< getChildren) -< subsection
    table       <- getTable <<< getChildren -< subsection
    arrF standardSubsectionFromTable -<
      (titleText, titleId, description, table)

standardSubsectionFromTable :: (String, String, String, Table XmlTree)
                               -> Maybe StandardSubsection
standardSubsectionFromTable
  (subsectionTitle, ident, description, table@(Table header _)) =
  do (subsectionNumber, name) <- parseSubsectionTitle subsectionTitle
     firstColName <- htmlToPlain =<< headMay header
     -- TODO: This should probably print a warning
     -- This fails on 3.25
     guard (name == firstColName)
     enumElements <- tableElements table
     guard (not . null $ enumElements)
     pure $ StandardSubsection subsectionNumber
                               (strip name)
                               ident
                               (strip description)
                               enumElements

-- | Extract the EnumElements from a table
tableElements :: Table XmlTree -> Maybe [EnumElement]
tableElements (Table header body) =
  do plainHeader <- traverse htmlToPlain header
     let hasRequiredCapability = "Required Capability" `elem` plainHeader ||
                                 "Depends On" `elem` plainHeader
         hasExtraOperands = "Extra Operands" `elem` plainHeader
     -- Get the EnumElement for this row
     let rowElement row = do
           enum <- readMaybe =<< htmlToPlain =<< row `atMay` 0
           nameAndDescription <- row `atMay` 1
           name <- headLine =<< htmlToPlain nameAndDescription
           guard (not . null $ name)
           description <- tailLines <$> htmlToHaddock nameAndDescription
           let rcColumn = 2
               -- TODO: There's some nasty mixing of Maybe meaning error and
               -- Maybe meaning optional parameter here
               requiredCapability = do guard hasRequiredCapability
                                       rc <- htmlToPlain =<< (row `atMay` rcColumn)
                                       guard (not . null $ rc)
                                       pure rc
               eoColumnStart = if hasRequiredCapability then 3 else 2
               extraOperands = if hasExtraOperands
                                 then catMaybes . fmap parseOperand . drop eoColumnStart $ row
                                 else []
           pure $ EnumElement enum name description requiredCapability extraOperands
     traverse rowElement body

parseOperand :: XmlTree -> Maybe Operand
parseOperand t = do p <- htmlToPlain t
                    ty <- headLine p
                    let desc = tailLines p
                    pure $ Operand ty desc

--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------

-- | Return just the subsection containing the instructions
isInstructionSubsection :: ArrowXml a => a XmlTree XmlTree
isInstructionSubsection = hasSubsectionId "_a_id_instructions_a_instructions"

-- | Get the instructions for a single instruction group subsubsection
getInstructionGroup :: ArrowXml a => a XmlTree InstructionGroup
getInstructionGroup =
  proc tree ->
    do subsubsection  <- isSubsubsection -< tree
       title          <- single (hasName "h4" <<< getChildren) -< subsubsection
       (number, name) <- arrF parseSubsubsectionTitle <<< arrF htmlToPlain -< title
       -- Every table in this subsubsection should describe a new instruction
       tables         <- listA (getHeadlessTable <<< getChildren) -< subsubsection
       -- We probably want to warn when a table couldn't be parsed
       instructions   <- arr (catMaybes . fmap tableInstruction) -< tables
       arr3 InstructionGroup -< (number, (name, instructions))

-- | Parse a table and extract the instruction
tableInstruction :: Table XmlTree -> Maybe Instruction
tableInstruction table =
    do -- The first box contains the name and description
       nameAndDescription <- table `atCell` (0, 0)
       -- The first line is the name
       name <- headLine =<< htmlToPlain nameAndDescription
       -- And the remainder is a description of the function
       description <- tailLines <$> htmlToHaddock nameAndDescription
       let -- The next cell in the first row is optional and contains the
           -- required capabilities for this instruction
           requiredCapabilities = asum $
             do rcText <- htmlToPlain =<< (table `atCell` (0, 1))
                let rcLines = lines rcText
                -- Check that this is actually a capabilities list
                guard . (=="Capability:") =<< headMay rcLines
                -- The rest of the lines are the capabilities
                tailMay rcLines
       -- The second (and last) row contains the info on the instruction
       infoRow <- table `atRow` 1
       plainInfoRow <- traverse htmlToPlain infoRow
       -- The first cell is the word count of the instruction
       (wordCount, isVariadic) <- parseWordCount =<< plainInfoRow `atMay` 0
       -- The second cell is the opcode
       opCode <- readMaybe =<< plainInfoRow `atMay` 1
       -- The operands are the remainder
       let operands = drop 2 infoRow
       -- If this instruction is variadic then parse the (more complicated)
       -- last cell separately
       variadicOperand <-
         if isVariadic
           then pure . parseVariadicOperand =<< lastMay operands
           else pure Nothing
       -- grab all the fixed operands
       fixedOperands <- sequence . fmap parseOperand
                        =<< if isVariadic
                              then initMay operands
                              else pure operands
       pure Instruction { instructionName = name
                        , instructionDescription = description
                        , instructionRequiredCapabilities = requiredCapabilities
                        , instructionWordCount = wordCount
                        , instructionOpCode = opCode
                        , instructionFixedOperands = fixedOperands
                        , instructionVariadicOperand = variadicOperand
                        }

-- | The variadic operands come in the following flavors:
--   "Literal String
--    name"
--   "type, type, …
--    name, name"
--   "Optional type
--    name"
-- TODO: There are more kinds!
parseVariadicOperand :: XmlTree -> Maybe VariadicOperand
parseVariadicOperand = parseMaybe vo <=< htmlToPlain
  where vo = do opType <- string "Literal String"
                skipSpace
                name <- takeText
                pure . VariadicOperandString $ Operand (unpack opType) (unpack name)
             <|>
             do opType <- takeTill (==',')
                anyChar >> skipSpace
                _ <- string opType
                anyChar >> skipSpace
                _ <- string "…"
                skipSpace
                name <- takeText
                pure . VariadicOperandRepeated $ Operand (unpack opType) (unpack name)
             <|>
             do _ <- string "Optional"
                skipSpace
                opType <- takeTill isEndOfLine
                skipSpace
                name <- takeText
                pure . VariadicOperandOptional $ Operand (unpack opType) (unpack name)

-- | parse the first cell of the info row for an instruction returning the word
-- count and whether or not the instruction is variable
parseWordCount :: String -> Maybe (Word16, Bool)
parseWordCount = parseMaybe $
  do wordCount <- decimal
     isVariadic <- isInfixOf "variable" . unpack <$> takeText
     pure (wordCount, isVariadic)

--------------------------------------------------------------------------------
-- Some extra stuff
-- TODO: Move this elsewhere
--------------------------------------------------------------------------------

-- Reading html constructs

--
-- Subsection
--

isSubsection :: ArrowXml a => a XmlTree XmlTree
isSubsection = hasAttrValue "class" (== "sect2")

getSubsectionId :: ArrowXml a => a XmlTree String
getSubsectionId = isSubsection /> single (hasName "h3") >>> getAttrValue "id"

hasSubsectionId :: ArrowXml a => String -> a XmlTree XmlTree
hasSubsectionId n = (getSubsectionId >>> isA (== n)) `guards` this

parseSubsectionTitle :: String -> Maybe ((Int, Int), String)
parseSubsectionTitle = parseMaybe $
  do n <- (,) <$> decimal <* "." <*> decimal <* "."
     skipSpace
     (n, ) . unpack <$> takeText

--
-- Subsubsection
--

isSubsubsection :: ArrowXml a => a XmlTree XmlTree
isSubsubsection = hasAttrValue "class" (== "sect3")

parseSubsubsectionTitle :: String -> Maybe ((Int, Int, Int), String)
parseSubsubsectionTitle = parseMaybe $
  do n <- (,,) <$> decimal <* "." <*> decimal <* "." <*> decimal <* "."
     skipSpace
     (n, ) . unpack <$> takeText


-- Parsing

-- | parseMarkedDecimal parses numbers in base 10 whose digits may have commas
-- between them
parseMarkedDecimal :: Integral a => String -> Maybe a
parseMarkedDecimal t =
  (fmap fromIntegral . (readMaybe :: String -> Maybe Integer) . concat) =<<
  parseMaybe (many1 digit `sepBy1` string ",") t
  where digit = satisfy isDigit

-- | parseInWords returns the first valid result after trying the parser on the
-- words in the string
parseInWords :: ReadP a -> String -> Maybe a
parseInWords p s = headMay $
  do word <- words s
     case longestParse p word of
       Nothing -> []
       Just (v, _) -> pure v

parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p = rightToMaybe . parseOnly p . pack

longestParse :: ReadP a -> String -> Maybe (a, String)
longestParse p s =
  case readP_to_S p s of
    []     -> Nothing
    parses -> Just (minimumBy (compare `on` length . snd) parses)

headLine :: String -> Maybe String
headLine = fmap strip . listToMaybe . lines

tailLines :: String -> String
tailLines = strip . dropWhile (/= '\n')

strip :: String -> String
strip = stripL . stripR

stripL :: String -> String
stripL = dropWhile isSpace

stripR :: String -> String
stripR = reverse . stripL . reverse

