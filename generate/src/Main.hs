{-# LANGUAGE LambdaCase #-}

module Main(main) where

import Parse
import Write

main :: IO ()
main =
  do specHtml <- getContents
     parseSpec specHtml >>= \case
       Nothing -> error "Unable to extract spec from html"
       Just spec -> writeSpecToFiles "SpirV" spec
