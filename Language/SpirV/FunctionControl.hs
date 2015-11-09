{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SpirV.FunctionControl where

import Data.Word (Word32)
import Language.SpirV.SpirEnum
import qualified Language.SpirV.Capability as Capability

data FunctionControl = None 
                     | Inline 
                     | DontInline 
                     | Pure 
                     | Const
  deriving(Read, Show, Eq, Ord)

instance SpirEnum FunctionControl Word32 where
  toWord None = 0
  toWord Inline = 1
  toWord DontInline = 2
  toWord Pure = 4
  toWord Const = 8

  fromWord 0 = Just None
  fromWord 1 = Just Inline
  fromWord 2 = Just DontInline
  fromWord 4 = Just Pure
  fromWord 8 = Just Const
  fromWord _ = Nothing

  
  requiredCapabilities _ = []
