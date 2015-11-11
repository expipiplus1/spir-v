module Control.Arrow.ArrowList.Extra
  ( module Control.Arrow.ArrowList
  , arrF
  ) where

import Control.Arrow.ArrowList
import Data.Foldable (toList)

arrF :: (ArrowList a, Foldable t) => (b -> t c) -> a b c
arrF f = arrL (toList . f)
