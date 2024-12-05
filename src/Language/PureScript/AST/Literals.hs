{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- The core functional representation for literal values.
module Language.PureScript.AST.Literals where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import GHC.Generics (Generic)
import Language.PureScript.PSString (PSString)
import Prelude

-- |
-- Data type for literal values. Parameterised so it can be used for Exprs and
-- Binders.
data Literal a
  = -- |
    -- A numeric literal
    NumericLiteral (Either Integer Double)
  | -- |
    -- A string literal
    StringLiteral PSString
  | -- |
    -- A character literal
    CharLiteral Char
  | -- |
    -- A boolean literal
    BooleanLiteral Bool
  | -- |
    -- An array literal
    ArrayLiteral [a]
  | -- |
    -- An object literal
    ObjectLiteral [(PSString, a)]
  deriving (Data, Eq, Ord, Show, Functor, Generic, NFData)
