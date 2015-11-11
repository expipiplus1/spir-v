module Data.Maybe.Extra
  ( module Data.Maybe
  , rightToMaybe
  ) where

import Data.Maybe

rightToMaybe :: Either t a -> Maybe a
rightToMaybe (Right x) = Just x
rightToMaybe (Left _) = Nothing
