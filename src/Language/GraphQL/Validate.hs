{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}

-- | GraphQL validator.
module Language.GraphQL.Validate
    ( Error(..)
    , document
    , module Language.GraphQL.Validate.Rules
    ) where

import Control.Monad (join)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (runReaderT)
import Data.Sequence (Seq(..), (><), (|>))
import qualified Data.Sequence as Seq
import Language.GraphQL.AST.Document
import Language.GraphQL.Type.Internal
import Language.GraphQL.Type.Schema (Schema(..))
import Language.GraphQL.Validate.Rules
import Language.GraphQL.Validate.Validation

-- | Validates a document and returns a list of found errors. If the returned
-- list is empty, the document is valid.
document :: forall m. Schema m -> [Rule m] -> Document -> Seq Error
document schema' rules' document' =
    runReaderT reader context
  where
    context = Validation
        { ast = document'
        , schema = schema'
        , types = collectReferencedTypes schema'
        }
    reader = do
        rule' <- lift $ Seq.fromList rules'
        join $ lift $ foldr (definition rule') Seq.empty document'

definition :: Rule m -> Definition -> Seq (RuleT m) -> Seq (RuleT m)
definition (DefinitionRule rule) definition' acc =
    acc |> rule definition'
definition rule (ExecutableDefinition executableDefinition') acc =
    acc >< executableDefinition rule executableDefinition'
definition _ _ acc = acc

executableDefinition :: Rule m -> ExecutableDefinition -> Seq (RuleT m)
executableDefinition rule (DefinitionOperation operation) =
    operationDefinition rule operation
executableDefinition rule (DefinitionFragment fragment) =
    fragmentDefinition rule fragment

operationDefinition :: Rule m -> OperationDefinition -> Seq (RuleT m)
operationDefinition (OperationDefinitionRule rule) operationDefinition' =
    pure $ rule operationDefinition'
operationDefinition rule (SelectionSet selections _) =
    selectionSet rule selections
operationDefinition rule (OperationDefinition _ _ _ _ selections _) =
    selectionSet rule selections

fragmentDefinition :: Rule m -> FragmentDefinition -> Seq (RuleT m)
fragmentDefinition (FragmentDefinitionRule rule) fragmentDefinition' =
    pure $ rule fragmentDefinition'
fragmentDefinition rule fragmentDefinition'@(FragmentDefinition _ _ _ selections _)
    | FragmentRule definitionRule _ <- rule =
        applyToChildren |> definitionRule fragmentDefinition'
    | otherwise = applyToChildren
  where
    applyToChildren = selectionSet rule selections

selectionSet :: Traversable t => Rule m -> t Selection -> Seq (RuleT m)
selectionSet = foldMap . selection

selection :: Rule m -> Selection -> Seq (RuleT m)
selection rule selection'
    | SelectionRule selectionRule <- rule =
        applyToChildren |> selectionRule selection'
    | otherwise = applyToChildren
  where
    applyToChildren =
        case selection' of
            FieldSelection field' -> field rule field'
            InlineFragmentSelection inlineFragment' ->
                inlineFragment rule inlineFragment'
            FragmentSpreadSelection fragmentSpread' ->
                pure $ fragmentSpread rule fragmentSpread'

field :: Rule m -> Field -> Seq (RuleT m)
field (FieldRule rule) field' = pure $ rule field'
field rule (Field _ _ _ _ selections _) = selectionSet rule selections

inlineFragment :: Rule m -> InlineFragment -> Seq (RuleT m)
inlineFragment (FragmentRule _ rule) inlineFragment' =
    pure $ rule inlineFragment'
inlineFragment rule (InlineFragment _ _ selections _) =
    selectionSet rule selections

fragmentSpread :: Rule m -> FragmentSpread -> RuleT m
fragmentSpread (FragmentSpreadRule rule) fragmentSpread' = rule fragmentSpread'
fragmentSpread _ _ = lift mempty
