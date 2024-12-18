{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.AST.Declarations.ChainId
  ( ChainId,
    mkChainId,
  )
where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Data (Data)
import Language.PureScript.AST.SourcePos qualified as Pos
import Prelude

-- |
-- For a given instance chain, stores the chain's file name and
-- the starting source pos of the first instance in the chain.
-- This data is used to determine which instances are part of
-- the same instance chain.
newtype ChainId = ChainId (String, Pos.SourcePos)
  deriving (Data, Eq, Ord, Show, NFData, Serialise)

mkChainId :: String -> Pos.SourcePos -> ChainId
mkChainId fileName startingSourcePos = ChainId (fileName, startingSourcePos)
