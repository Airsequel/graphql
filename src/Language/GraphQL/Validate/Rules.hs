{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module contains default rules defined in the GraphQL specification.
module Language.GraphQL.Validate.Rules
    ( executableDefinitionsRule
    , loneAnonymousOperationRule
    , singleFieldSubscriptionsRule
    , specifiedRules
    , uniqueOperationNamesRule
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
    , uniqueOperationNamesRule
    ]

-- | Definition must be OperationDefinition or FragmentDefinition.
executableDefinitionsRule :: forall m. Rule m
executableDefinitionsRule = DefinitionRule $ \case
    ExecutableDefinition _ -> lift Nothing
    TypeSystemDefinition _ location -> pure $ error' location
    TypeSystemExtension _ location -> pure $ error' location
  where
    error' location = Error
        { message =
            "Definition must be OperationDefinition or FragmentDefinition."
        , locations = [location]
        , path = []
        }

-- | Subscription operations must have exactly one root field.
singleFieldSubscriptionsRule :: forall m. Rule m
singleFieldSubscriptionsRule = OperationDefinitionRule $ \case
    OperationDefinition Subscription name' _ _ rootFields location -> do
        groupedFieldSet <- evalStateT (collectFields rootFields) HashSet.empty
        case HashSet.size groupedFieldSet of
            1 -> lift Nothing
            _
                | Just name <- name' -> pure $ Error
                    { message = unwords
                        [ "Subscription"
                        , Text.unpack name
                        , "must select only one top level field."
                        ]
                    , locations = [location]
                    , path = []
                    }
                | otherwise -> pure $ Error
                    { message = errorMessage
                    , locations = [location]
                    , path = []
                    }
    _ -> lift Nothing
  where
    errorMessage =
        "Anonymous Subscription must select only one top level field."
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
          | (viewOperation -> Just operationDefinition) <- definition =
              compareAnonymousOperations thisLocation operationDefinition
      filterAnonymousOperations _ _ accumulator = accumulator
      compareAnonymousOperations thisLocation = \case
          OperationDefinition _ _ _ _ _ thatLocation
              | thisLocation /= thatLocation -> pure $ error' thisLocation
          SelectionSet _ thatLocation
              | thisLocation /= thatLocation -> pure $ error' thisLocation
          _ -> Nothing
      error' location = Error
          { message =
              "This anonymous operation must be the only defined operation."
          , locations = [location]
          , path = []
          }

-- | Each named operation definition must be unique within a document when
-- referred to by its name.
uniqueOperationNamesRule :: forall m. Rule m
uniqueOperationNamesRule = OperationDefinitionRule $ \case
    OperationDefinition _ (Just thisName) _ _ _ thisLocation -> do
        ast' <- asks ast
        let locations' = foldr (filterByName thisName) [] ast'
        if length locations' > 1 && head locations' == thisLocation
            then pure $ error' thisName locations'
            else lift Nothing
    _ -> lift Nothing
  where
    error' operationName locations' = Error 
        { message = concat
            [ "There can be only one operation named \""
            , Text.unpack operationName
            , "\"."
            ]
        , locations = locations'
        , path = []
        }
    filterByName thisName definition' accumulator
        | (viewOperation -> Just operationDefinition) <- definition'
        , OperationDefinition _ (Just thatName) _ _ _ thatLocation <- operationDefinition
        , thisName == thatName = thatLocation : accumulator
        | otherwise = accumulator

viewOperation :: Definition -> Maybe OperationDefinition
viewOperation definition
    | ExecutableDefinition executableDefinition <- definition
    , DefinitionOperation operationDefinition <- executableDefinition =
        Just operationDefinition
viewOperation _ = Nothing
