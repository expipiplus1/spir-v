module ConvertHTML
  ( htmlToPlain
  , htmlToHaddock
  ) where

import Data.Maybe.Extra (rightToMaybe)
import Regex
import Text.Pandoc (readHtml, writePlain, writeHaddock, def, WriterOptions(..))
import Text.Pandoc.Walk (Walkable(walk))
import Text.Pandoc.Definition (Pandoc, Inline(..), nullAttr)
import Text.XML.HXT.Core hiding (xshow)
import Text.XML.HXT.DOM.ShowXml (xshow)

--
-- converters
--

writerOptions :: WriterOptions
writerOptions = def{ writerWrapText = False }

htmlToPlain :: XmlTree -> Maybe String
htmlToPlain = rightToMaybe . fmap (writePlain writerOptions . pandocStripEmph) . readHtml def . showXml


htmlToHaddock :: XmlTree -> Maybe String
htmlToHaddock =
  rightToMaybe .
  fmap (removeTags . fixLinks . pandoc2507 . writeHaddock writerOptions) .
  readHtml def .
  showXml

--
-- Reading the xml
--

-- Get the (patched) xml string
showXml :: XmlTree -> String
showXml = pandoc2512 . xshow . pure

--
-- Change the pandoc output slightly
--

-- | Remove formatting on labels in links: https://github.com/jgm/pandoc/issues/2507
pandoc2507 :: String -> String
pandoc2507 = regexReplace "<([^ >]+) __([^>]+)__>" "__<\\1 \\2>__" .
             regexReplace "<([^ >]+) /([^>]+)/>" "/<\\1 \\2>/"

-- | Escape the angled brackets in <id>
pandoc2512 :: String -> String
pandoc2512 = regexReplace "<em><id/?>(s?)</em>" "<em>&lt;id&gt;\\1</em>"

pandocStripEmph :: Pandoc -> Pandoc
pandocStripEmph = walk go
  where go (Emph is) = Span nullAttr is
        go (Strong is) = Span nullAttr is
        go (SmallCaps is) = Span nullAttr is
        go i = i

fixLinks :: String -> String
fixLinks = regexReplace "<([^ >]+) ([^>]+)>" "<https:\\/\\/www.khronos.org\\/registry\\/spir-v\\/specs\\/1.0\\/SPIRV.html\\1 \\2>"

removeTags :: String -> String
removeTags = regexReplace "#([a-zA-Z0-9]+)#__[a-zA-Z0-9]+__" "\\1"

