{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}

-- | GraphQL validator.
module Language.GraphQL.Validate
    ( Error(..)
    , Path(..)
    , document
    , module Language.GraphQL.Validate.Rules
    ) where

import Control.Monad.Trans.Reader (Reader, asks, runReader)
import Data.Foldable (foldrM)
import Data.Sequence (Seq(..), (><), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Language.GraphQL.AST.Document
import Language.GraphQL.Type.Schema
import Language.GraphQL.Validate.Rules

data Context m = Context
    { ast :: Document
    , schema :: Schema m
    , rules :: [Rule]
    }

type ValidateT m = Reader (Context m) (Seq Error)

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

-- | Validates a document and returns a list of found errors. If the returned
-- list is empty, the document is valid.
document :: forall m. Schema m -> [Rule] -> Document -> Seq Error
document schema' rules' document' =
    runReader (foldrM go Seq.empty document') context
  where
    context = Context
        { ast = document'
        , schema = schema'
        , rules = rules'
        }
    go definition' accumulator = (accumulator ><) <$> definition definition'

definition :: forall m. Definition -> ValidateT m
definition = \case
    definition'@(ExecutableDefinition executableDefinition' _) -> do
        applied <- applyRules definition'
        children <- executableDefinition executableDefinition'
        pure $ children >< applied
    definition' -> applyRules definition'
  where
    applyRules definition' = foldr (ruleFilter definition') Seq.empty
        <$> asks rules
    ruleFilter definition' (DefinitionRule rule) accumulator
        | Just message' <- rule definition' =
            accumulator |> Error
                { message = message'
                , locations = [definitionLocation definition']
                , path = []
                }
        | otherwise = accumulator
    definitionLocation (ExecutableDefinition _ location) = location
    definitionLocation (TypeSystemDefinition _ location) = location
    definitionLocation (TypeSystemExtension _ location) = location

executableDefinition :: forall m. ExecutableDefinition -> ValidateT m
executableDefinition (DefinitionOperation definition') =
    operationDefinition definition'
executableDefinition (DefinitionFragment definition') =
    fragmentDefinition definition'

operationDefinition :: forall m. OperationDefinition -> ValidateT m
operationDefinition (SelectionSet _operation) =
    pure Seq.empty
operationDefinition (OperationDefinition _type _name _variables _directives _selection) =
    pure Seq.empty

fragmentDefinition :: forall m. FragmentDefinition -> ValidateT m
fragmentDefinition _fragment = pure Seq.empty