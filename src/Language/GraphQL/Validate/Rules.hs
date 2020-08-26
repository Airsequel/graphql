{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains default rules defined in the GraphQL specification.
module Language.GraphQL.Validate.Rules
    ( executableDefinitionsRule
    , specifiedRules
    ) where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (asks)
import Control.Monad.Trans.State (evalStateT, gets, modify)
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Language.GraphQL.AST.Document
import Language.GraphQL.Type.Internal
import qualified Language.GraphQL.Type.Schema as Schema
import Language.GraphQL.Validate.Validation

-- | Default rules given in the specification.
specifiedRules :: forall m. [Rule m]
specifiedRules =
    [ executableDefinitionsRule
    , singleFieldSubscriptionsRule
    , loneAnonymousOperationRule
    ]

-- | Definition must be OperationDefinition or FragmentDefinition.
executableDefinitionsRule :: forall m. Rule m
executableDefinitionsRule = DefinitionRule $ \case
    ExecutableDefinition _ -> lift Nothing
    _ -> pure "Definition must be OperationDefinition or FragmentDefinition."

-- | Subscription operations must have exactly one root field.
singleFieldSubscriptionsRule :: forall m. Rule m
singleFieldSubscriptionsRule = OperationDefinitionRule $ \case
    OperationDefinition Subscription name' _ _ rootFields _ -> do
        groupedFieldSet <- evalStateT (collectFields rootFields) HashSet.empty
        case HashSet.size groupedFieldSet of
            1 -> lift Nothing
            _
                | Just name <- name' -> pure $ unwords
                    [ "Subscription"
                    , Text.unpack name
                    , "must select only one top level field."
                    ]
                | otherwise -> pure
                    "Anonymous Subscription must select only one top level field."
    _ -> lift Nothing
  where
    collectFields selectionSet = foldM forEach HashSet.empty selectionSet
    forEach accumulator (Field alias name _ directives _)
        | any skip directives = pure accumulator
        | Just aliasedName <- alias = pure
            $ HashSet.insert aliasedName accumulator
        | otherwise = pure $ HashSet.insert name accumulator
    forEach accumulator (FragmentSpread fragmentName directives)
        | any skip directives = pure accumulator
        | otherwise = do
            inVisitetFragments <- gets $ HashSet.member fragmentName
            if inVisitetFragments
               then pure accumulator
               else collectFromSpread fragmentName accumulator
    forEach accumulator (InlineFragment typeCondition' directives selectionSet)
        | any skip directives = pure accumulator
        | Just typeCondition <- typeCondition' =
            collectFromFragment typeCondition selectionSet accumulator
        | otherwise = HashSet.union accumulator
            <$> collectFields selectionSet
    skip (Directive "skip" [Argument "if" (Boolean True)]) = True
    skip (Directive "include" [Argument "if" (Boolean False)]) = True
    skip _ = False
    findFragmentDefinition (ExecutableDefinition executableDefinition) Nothing
        | DefinitionFragment fragmentDefinition <- executableDefinition =
            Just fragmentDefinition
    findFragmentDefinition _ accumulator = accumulator
    collectFromFragment typeCondition selectionSet accumulator = do
        types' <- lift $ asks types
        schema' <- lift $ asks schema
        case lookupTypeCondition typeCondition types' of
            Nothing -> pure accumulator
            Just compositeType
                | Just objectType <- Schema.subscription schema'
                , True <- doesFragmentTypeApply compositeType objectType ->
                    HashSet.union accumulator<$> collectFields selectionSet
                | otherwise -> pure accumulator
    collectFromSpread fragmentName accumulator = do
        modify $ HashSet.insert fragmentName
        ast' <- lift $ asks ast
        case foldr findFragmentDefinition Nothing ast' of
            Nothing -> pure accumulator
            Just (FragmentDefinition _ typeCondition _ selectionSet _) ->
                collectFromFragment typeCondition selectionSet accumulator

-- | GraphQL allows a short‐hand form for defining query operations when only
-- that one operation exists in the document.
loneAnonymousOperationRule :: forall m. Rule m
loneAnonymousOperationRule = OperationDefinitionRule $ \case
      SelectionSet _ thisLocation -> check thisLocation
      OperationDefinition _ Nothing _ _ _ thisLocation -> check thisLocation
      _ -> lift Nothing
    where
      check thisLocation = asks ast
        >>= lift . foldr (filterAnonymousOperations thisLocation) Nothing
      filterAnonymousOperations thisLocation definition Nothing
          | ExecutableDefinition executableDefinition <- definition
          , DefinitionOperation operationDefinition <- executableDefinition =
              compareAnonymousOperations thisLocation operationDefinition
      filterAnonymousOperations _ _ accumulator = accumulator
      compareAnonymousOperations thisLocation operationDefinition
          | OperationDefinition _ _ _ _ _ thatLocation <- operationDefinition
          , thisLocation /= thatLocation = pure message
          | SelectionSet _ thatLocation <- operationDefinition
          , thisLocation /= thatLocation = pure message
      compareAnonymousOperations _ _ = Nothing
      message = "This anonymous operation must be the only defined operation."
