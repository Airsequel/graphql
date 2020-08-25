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

import Control.Monad (foldM)
import Control.Monad.Trans.Reader (Reader, asks, mapReaderT, runReader)
import Data.Foldable (foldrM)
import Data.Sequence (Seq(..), (><), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Language.GraphQL.AST.Document
import Language.GraphQL.Type.Internal
import Language.GraphQL.Type.Schema (Schema(..))
import Language.GraphQL.Validate.Rules
import Language.GraphQL.Validate.Validation

type ValidateT m = Reader (Validation m) (Seq Error)

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
document :: forall m. Schema m -> [Rule m] -> Document -> Seq Error
document schema' rules' document' =
    runReader (foldrM go Seq.empty document') context
  where
    context = Validation
        { ast = document'
        , schema = schema'
        , types = collectReferencedTypes schema'
        , rules = rules'
        }
    go definition' accumulator = (accumulator ><) <$> definition definition'

definition :: forall m. Definition -> ValidateT m
definition = \case
    definition'@(ExecutableDefinition executableDefinition') -> do
        applied <- applyRules definition'
        children <- executableDefinition executableDefinition'
        pure $ children >< applied
    definition' -> applyRules definition'
  where
    applyRules definition' =
        asks rules >>= foldM (ruleFilter definition') Seq.empty
    ruleFilter definition' accumulator (DefinitionRule rule) =
        flip mapReaderT (rule definition') $ \case
            Just message' ->
                pure $ accumulator |> Error
                    { message = message'
                    , locations = [definitionLocation definition']
                    , path = []
                    }
            Nothing -> pure accumulator
    ruleFilter _ accumulator _ = pure accumulator
    definitionLocation (ExecutableDefinition executableDefinition')
        | DefinitionOperation definitionOperation <- executableDefinition'
        , SelectionSet _ location <- definitionOperation = location
        | DefinitionOperation definitionOperation <- executableDefinition'
        , OperationDefinition _ _ _ _ _ location <- definitionOperation =
            location
        | DefinitionFragment fragmentDefinition' <- executableDefinition'
        , FragmentDefinition _ _ _ _ location <- fragmentDefinition' = location
    definitionLocation (TypeSystemDefinition _ location) = location
    definitionLocation (TypeSystemExtension _ location) = location

executableDefinition :: forall m. ExecutableDefinition -> ValidateT m
executableDefinition (DefinitionOperation definition') =
    operationDefinition definition'
executableDefinition (DefinitionFragment definition') =
    fragmentDefinition definition'

operationDefinition :: forall m. OperationDefinition -> ValidateT m
operationDefinition operation =
    asks rules >>= foldM (ruleFilter operation) Seq.empty
  where
    ruleFilter definition' accumulator (OperationDefinitionRule rule) =
        flip mapReaderT (rule definition') $ \case
            Just message' ->
                pure $ accumulator |> Error
                    { message = message'
                    , locations = [definitionLocation operation]
                    , path = []
                    }
            Nothing -> pure accumulator
    ruleFilter _ accumulator _ = pure accumulator
    definitionLocation (SelectionSet _ location) = location
    definitionLocation (OperationDefinition _ _ _ _ _ location) = location

fragmentDefinition :: forall m. FragmentDefinition -> ValidateT m
fragmentDefinition _fragment = pure Seq.empty
