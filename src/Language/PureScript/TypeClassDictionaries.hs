{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.TypeClassDictionaries where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Language.PureScript.AST.Declarations.ChainId (ChainId)
import Language.PureScript.Names (Ident, ProperName (..), ProperNameType (..), Qualified, disqualify)
import Language.PureScript.Types (SourceConstraint, SourceType)
import Prelude

--
-- Data representing a type class dictionary which is in scope
--
data TypeClassDictionaryInScope v = TypeClassDictionaryInScope
  { -- | The instance chain
    tcdChain :: Maybe ChainId,
    -- | Index of the instance chain
    tcdIndex :: Integer,
    -- | The value with which the dictionary can be accessed at runtime
    tcdValue :: v,
    -- | How to obtain this instance via superclass relationships
    tcdPath :: [(Qualified (ProperName 'ClassName), Integer)],
    -- | The name of the type class to which this type class instance applies
    tcdClassName :: Qualified (ProperName 'ClassName),
    -- | Quantification of type variables in the instance head and dependencies
    tcdForAll :: [(Text, SourceType)],
    -- | The kinds to which this type class instance applies
    tcdInstanceKinds :: [SourceType],
    -- | The types to which this type class instance applies
    tcdInstanceTypes :: [SourceType],
    -- | Type class dependencies which must be satisfied to construct this dictionary
    tcdDependencies :: Maybe [SourceConstraint],
    -- | If this instance was unnamed, the type to use when describing it in
    -- error messages
    tcdDescription :: Maybe SourceType
  }
  deriving (Data, Show, Functor, Foldable, Traversable, Generic)

instance (NFData v) => NFData (TypeClassDictionaryInScope v)

type NamedDict = TypeClassDictionaryInScope (Qualified Ident)

-- | Generate a name for a superclass reference which can be used in
-- generated code.
superclassName :: Qualified (ProperName 'ClassName) -> Integer -> Text
superclassName pn index = runProperName (disqualify pn) <> pack (show index)
