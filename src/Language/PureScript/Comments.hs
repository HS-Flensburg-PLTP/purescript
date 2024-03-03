{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Defines the types of source code comments
module Language.PureScript.Comments where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson.TH (Options (..), SumEncoding (..), defaultOptions, deriveJSON)
import Data.Data (Data)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

data Comment
  = LineComment Text
  | BlockComment Text
  deriving (Data, Show, Eq, Ord, Generic)

instance NFData Comment

instance Serialise Comment

$(deriveJSON (defaultOptions {sumEncoding = ObjectWithSingleField}) ''Comment)
