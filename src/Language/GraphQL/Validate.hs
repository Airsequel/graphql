{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}

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
definition definition'
    | ExecutableDefinition executableDefinition' <- definition'
        = visitChildSelections ruleFilter
        $ executableDefinition executableDefinition'
    | otherwise = asks rules >>= foldM ruleFilter Seq.empty
  where
    ruleFilter accumulator (DefinitionRule rule) =
        mapReaderT (runRule accumulator) $ rule definition'
    ruleFilter accumulator _ = pure accumulator

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
     in visitChildSelections ruleFilter $ traverseSelectionSet selectionSet
  where
    ruleFilter accumulator (OperationDefinitionRule rule) =
        mapReaderT (runRule accumulator) $ rule operation
    ruleFilter accumulator _ = pure accumulator
    getSelectionSet (SelectionSet selectionSet _) = selectionSet
    getSelectionSet (OperationDefinition _ _ _ _ selectionSet _) = selectionSet

visitChildSelections :: forall  m
    . (Seq Error -> Rule m -> ValidateT m)
    -> ValidateT m
    -> ValidateT m
visitChildSelections ruleFilter children' = do
    rules' <- asks rules
    applied <- foldM ruleFilter Seq.empty rules'
    children <- children'
    pure $ children >< applied

selection :: forall m. Selection -> ValidateT m
selection selection'
    | FragmentSpreadSelection fragmentSelection <- selection' =
        visitChildSelections ruleFilter $ fragmentSpread fragmentSelection
    | Field _ _ _ _ selectionSet _ <- selection' =
        visitChildSelections ruleFilter $ traverseSelectionSet selectionSet
    | InlineFragmentSelection fragmentSelection <- selection' =
        visitChildSelections ruleFilter $ inlineFragment fragmentSelection
  where
    ruleFilter accumulator (SelectionRule rule) =
        mapReaderT (runRule accumulator) $ rule selection'
    ruleFilter accumulator _ = pure accumulator

inlineFragment :: forall m. InlineFragment -> ValidateT m
inlineFragment fragment@(InlineFragment _ _ selections _) =
    visitChildSelections ruleFilter $ traverseSelectionSet selections
  where
    ruleFilter accumulator (FragmentRule _ inlineRule) =
        mapReaderT (runRule accumulator) $ inlineRule fragment
    ruleFilter accumulator _ = pure accumulator

fragmentSpread :: forall m. FragmentSpread -> ValidateT m
fragmentSpread fragment =
    asks rules >>= foldM ruleFilter Seq.empty
  where
    ruleFilter accumulator (FragmentSpreadRule rule) =
        mapReaderT (runRule accumulator) $ rule fragment
    ruleFilter accumulator _ = pure accumulator

traverseSelectionSet :: Traversable t => forall m. t Selection -> ValidateT m
traverseSelectionSet = fmap fold . traverse selection

fragmentDefinition :: forall m. FragmentDefinition -> ValidateT m
fragmentDefinition fragment@(FragmentDefinition _ _ _ selectionSet _) =
    visitChildSelections ruleFilter $ traverseSelectionSet selectionSet
  where
    ruleFilter accumulator (FragmentDefinitionRule rule) =
        mapReaderT (runRule accumulator) $ rule fragment
    ruleFilter accumulator (FragmentRule definitionRule _) =
        mapReaderT (runRule accumulator) $ definitionRule fragment
    ruleFilter accumulator _ = pure accumulator
