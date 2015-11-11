{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Table
  ( Table(..)
  , atRow
  , atCell
  , isTable
  , getTable
  , getHeadlessTable
  ) where

import Safe (atMay)
import Text.XML.HXT.Core

data Table a = Table { tableHead :: [a]
                     , tableBody :: [[a]]
                     }
  deriving (Show, Eq, Functor, Foldable, Traversable)

atRow :: Table a -> Int -> Maybe [a]
atRow t r = tableBody t `atMay` r

atCell :: Table a -> (Int, Int) -> Maybe a
atCell t (r, c) = (t `atRow` r) >>= (`atMay` c)

isTable :: ArrowXml a => a XmlTree XmlTree
isTable = hasName "table"

getTable :: ArrowXml a => a XmlTree (Table XmlTree)
getTable =
  isTable >>>
  ((getChildren >>> hasName "thead" /> getTableRow) &&&
   (getChildren >>> hasName "tbody" >>> listA (getChildren >>> getTableRow))) >>^
  uncurry Table
  where getTableRow =
          hasName "tr" >>>
          listA (getChildren >>> (hasName "td" `orElse` hasName "th"))

getHeadlessTable :: ArrowXml a => a XmlTree (Table XmlTree)
getHeadlessTable =
  isTable />
  (hasName "tbody" >>> listA (getChildren >>> getTableRow)) >>^
  Table []
  where getTableRow = hasName "tr" >>> listA (getChildren >>> hasName "td")
