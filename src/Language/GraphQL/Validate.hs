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
import Data.Foldable (fold, foldrM)
import Data.Sequence (Seq(..), (><), (|>))
import qualified Data.Sequence as Seq
import Language.GraphQL.AST.Document
import Language.GraphQL.Type.Internal
import Language.GraphQL.Type.Schema (Schema(..))
import Language.GraphQL.Validate.Rules
import Language.GraphQL.Validate.Validation

type ValidateT m = Reader (Validation m) (Seq Error)

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
        mapReaderT (runRule accumulator) $ rule definition'
    ruleFilter _ accumulator _ = pure accumulator

runRule :: Applicative f => Seq Error -> Maybe Error -> f (Seq Error)
runRule accumulator (Just error') = pure $ accumulator |> error'
runRule accumulator Nothing = pure accumulator

executableDefinition :: forall m. ExecutableDefinition -> ValidateT m
executableDefinition (DefinitionOperation definition') =
    operationDefinition definition'
executableDefinition (DefinitionFragment definition') =
    fragmentDefinition definition'

operationDefinition :: forall m. OperationDefinition -> ValidateT m
operationDefinition operation =
    let selectionSet = getSelectionSet operation
     in visitChildSelections ruleFilter selectionSet
  where
    ruleFilter accumulator (OperationDefinitionRule rule) =
        mapReaderT (runRule accumulator) $ rule operation
    ruleFilter accumulator _ = pure accumulator
    getSelectionSet (SelectionSet selectionSet _) = selectionSet
    getSelectionSet (OperationDefinition _ _ _ _ selectionSet _) = selectionSet

selection :: forall m. Selection -> ValidateT m
selection selection'@FragmentSpread{} =
    asks rules >>= foldM ruleFilter Seq.empty
  where
    ruleFilter accumulator (SelectionRule rule) =
        mapReaderT (runRule accumulator) $ rule selection'
    ruleFilter accumulator _ = pure accumulator
selection (Field _ _ _ _ selectionSet) = traverseSelectionSet selectionSet
selection (InlineFragment _ _ selectionSet) = traverseSelectionSet selectionSet

traverseSelectionSet :: Traversable t => forall m. t Selection -> ValidateT m
traverseSelectionSet = fmap fold . traverse selection

visitChildSelections :: Traversable t
    => (Seq Error -> Rule m -> ValidateT m)
    -> t Selection
    -> ValidateT m
visitChildSelections ruleFilter selectionSet = do
    rules' <- asks rules
    applied <- foldM ruleFilter Seq.empty rules'
    children <- traverseSelectionSet selectionSet
    pure $ children >< applied

fragmentDefinition :: forall m. FragmentDefinition -> ValidateT m
fragmentDefinition fragment@(FragmentDefinition _ _ _ selectionSet _) =
    visitChildSelections ruleFilter selectionSet
  where
    ruleFilter accumulator (FragmentDefinitionRule rule) =
        mapReaderT (runRule accumulator) $ rule fragment
    ruleFilter accumulator _ = pure accumulator
