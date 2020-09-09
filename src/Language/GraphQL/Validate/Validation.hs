{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

-- | Definitions used by the validation rules and the validator itself.
module Language.GraphQL.Validate.Validation
    ( Error(..)
    , Path(..)
    , Rule(..)
    , RuleT
    , Validation(..)
    ) where

import Control.Monad.Trans.Reader (ReaderT(..))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Language.GraphQL.AST.Document
import Language.GraphQL.Type.Schema (Schema)
import qualified Language.GraphQL.Type.Schema as Schema

-- | If an error can be associated to a particular field in the GraphQL result,
-- it must contain an entry with the key path that details the path of the
-- response field which experienced the error. This allows clients to identify
-- whether a null result is intentional or caused by a runtime error.
data Path
    = Segment Text -- ^ Field name.
    | Index Int -- ^ List index if a field returned a list.
    deriving (Eq, Show)

-- | Validation error.
data Error = Error
    { message :: String
    , locations :: [Location]
    , path :: [Path]
    } deriving (Eq, Show)

-- | Validation rule context.
data Validation m = Validation
    { ast :: Document
    , schema :: Schema m
    , types :: HashMap Name (Schema.Type m)
    , rules :: [Rule m]
    }

-- | 'Rule' assigns a function to each AST node that can be validated. If the
-- validation fails, the function should return an error message, or 'Nothing'
-- otherwise.
data Rule m
    = DefinitionRule (Definition -> RuleT m)
    | OperationDefinitionRule (OperationDefinition -> RuleT m)
    | FragmentDefinitionRule (FragmentDefinition -> RuleT m)
    | SelectionRule (Selection -> RuleT m)
    | FragmentRule (FragmentDefinition -> RuleT m) (InlineFragment -> RuleT m)
    | FragmentSpreadRule (FragmentSpread -> RuleT m)
    | FieldRule (Field -> RuleT m)

-- | Monad transformer used by the rules.
type RuleT m = ReaderT (Validation m) Maybe Error
