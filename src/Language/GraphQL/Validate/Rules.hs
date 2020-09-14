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
    , fragmentsOnCompositeTypesRule
    , fragmentSpreadTargetDefinedRule
    , fragmentSpreadTypeExistenceRule
    , loneAnonymousOperationRule
    , noFragmentCyclesRule
    , noUnusedFragmentsRule
    , singleFieldSubscriptionsRule
    , specifiedRules
    , uniqueFragmentNamesRule
    , uniqueOperationNamesRule
    ) where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, asks)
import Control.Monad.Trans.State (StateT, evalStateT, gets, modify)
import Data.Bifunctor (first)
import Data.Foldable (find)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HashSet
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Sequence (Seq(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Language.GraphQL.AST.Document
import Language.GraphQL.Type.Internal
import qualified Language.GraphQL.Type.Schema as Schema
import Language.GraphQL.Validate.Validation

-- | Default rules given in the specification.
specifiedRules :: forall m. [Rule m]
specifiedRules =
    -- Documents.
    [ executableDefinitionsRule
    -- Operations.
    , singleFieldSubscriptionsRule
    , loneAnonymousOperationRule
    , uniqueOperationNamesRule
    -- Fragments.
    , uniqueFragmentNamesRule
    , fragmentSpreadTypeExistenceRule
    , fragmentsOnCompositeTypesRule
    , noUnusedFragmentsRule
    , fragmentSpreadTargetDefinedRule
    , noFragmentCyclesRule
    ]

-- | Definition must be OperationDefinition or FragmentDefinition.
executableDefinitionsRule :: forall m. Rule m
executableDefinitionsRule = DefinitionRule $ \case
    ExecutableDefinition _ -> lift mempty
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
            1 -> lift mempty
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
    _ -> lift mempty
  where
    errorMessage =
        "Anonymous Subscription must select only one top level field."
    collectFields selectionSet = foldM forEach HashSet.empty selectionSet
    forEach accumulator = \case
        FieldSelection fieldSelection -> forField accumulator fieldSelection
        FragmentSpreadSelection fragmentSelection ->
            forSpread accumulator fragmentSelection
        InlineFragmentSelection fragmentSelection ->
            forInline accumulator fragmentSelection
    forField accumulator (Field alias name _ directives _ _)
        | any skip directives = pure accumulator
        | Just aliasedName <- alias = pure
            $ HashSet.insert aliasedName accumulator
        | otherwise = pure $ HashSet.insert name accumulator
    forSpread accumulator (FragmentSpread fragmentName directives _)
        | any skip directives = pure accumulator
        | otherwise = do
            inVisitetFragments <- gets $ HashSet.member fragmentName
            if inVisitetFragments
               then pure accumulator
               else collectFromSpread fragmentName accumulator
    forInline accumulator (InlineFragment maybeType directives selections _)
        | any skip directives = pure accumulator
        | Just typeCondition <- maybeType =
            collectFromFragment typeCondition selections accumulator
        | otherwise = HashSet.union accumulator
            <$> collectFields selections
    skip (Directive "skip" [Argument "if" (Boolean True) _]) = True
    skip (Directive "include" [Argument "if" (Boolean False) _]) = True
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
                    HashSet.union accumulator <$> collectFields selectionSet
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
      _ -> lift mempty
    where
      check thisLocation = asks ast
          >>= lift . foldr (filterAnonymousOperations thisLocation) mempty
      filterAnonymousOperations thisLocation definition Empty
          | (viewOperation -> Just operationDefinition) <- definition =
              compareAnonymousOperations thisLocation operationDefinition
      filterAnonymousOperations _ _ accumulator = accumulator
      compareAnonymousOperations thisLocation = \case
          OperationDefinition _ _ _ _ _ thatLocation
              | thisLocation /= thatLocation -> pure $ error' thisLocation
          SelectionSet _ thatLocation
              | thisLocation /= thatLocation -> pure $ error' thisLocation
          _ -> mempty
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
    OperationDefinition _ (Just thisName) _ _ _ thisLocation ->
        findDuplicates (filterByName thisName) thisLocation (error' thisName)
    _ -> lift mempty
  where
    error' operationName = concat
        [ "There can be only one operation named \""
        , Text.unpack operationName
        , "\"."
        ]
    filterByName thisName definition' accumulator
        | (viewOperation -> Just operationDefinition) <- definition'
        , OperationDefinition _ (Just thatName) _ _ _ thatLocation <- operationDefinition
        , thisName == thatName = thatLocation : accumulator
        | otherwise = accumulator

findDuplicates :: (Definition -> [Location] -> [Location])
    -> Location
    -> String
    -> RuleT m
findDuplicates filterByName thisLocation errorMessage = do
    ast' <- asks ast
    let locations' = foldr filterByName [] ast'
    if length locations' > 1 && head locations' == thisLocation
        then pure $ error' locations'
        else lift mempty
  where
    error' locations' = Error 
        { message = errorMessage
        , locations = locations'
        , path = []
        }

viewOperation :: Definition -> Maybe OperationDefinition
viewOperation definition
    | ExecutableDefinition executableDefinition <- definition
    , DefinitionOperation operationDefinition <- executableDefinition =
        Just operationDefinition
viewOperation _ = Nothing

viewFragment :: Definition -> Maybe FragmentDefinition
viewFragment definition
    | ExecutableDefinition executableDefinition <- definition
    , DefinitionFragment fragmentDefinition <- executableDefinition =
        Just fragmentDefinition
viewFragment _ = Nothing

-- | Fragment definitions are referenced in fragment spreads by name. To avoid
-- ambiguity, each fragment’s name must be unique within a document.
--
-- Inline fragments are not considered fragment definitions, and are unaffected
-- by this validation rule.
uniqueFragmentNamesRule :: forall m. Rule m
uniqueFragmentNamesRule = FragmentDefinitionRule $ \case
    FragmentDefinition thisName _ _ _ thisLocation ->
        findDuplicates (filterByName thisName) thisLocation (error' thisName)
  where
    error' fragmentName = concat
        [ "There can be only one fragment named \""
        , Text.unpack fragmentName
        , "\"."
        ]
    filterByName thisName definition accumulator
        | Just fragmentDefinition <- viewFragment definition
        , FragmentDefinition thatName _ _ _ thatLocation <- fragmentDefinition
        , thisName == thatName = thatLocation : accumulator
        | otherwise = accumulator

-- | Named fragment spreads must refer to fragments defined within the document.
-- It is a validation error if the target of a spread is not defined.
fragmentSpreadTargetDefinedRule :: forall m. Rule m
fragmentSpreadTargetDefinedRule = FragmentSpreadRule $ \case
    FragmentSpread fragmentName _ location -> do
        ast' <- asks ast
        case find (isSpreadTarget fragmentName) ast' of
            Nothing -> pure $ Error
                { message = error' fragmentName
                , locations = [location]
                , path = []
                }
            Just _ -> lift mempty
  where
    error' fragmentName = concat
        [ "Fragment target \""
        , Text.unpack fragmentName
        , "\" is undefined."
        ]

isSpreadTarget :: Text -> Definition -> Bool
isSpreadTarget thisName (viewFragment -> Just fragmentDefinition)
    | FragmentDefinition thatName _ _ _ _ <- fragmentDefinition
    , thisName == thatName = True
isSpreadTarget _ _ = False

-- | Fragments must be specified on types that exist in the schema. This applies
-- for both named and inline fragments. If they are not defined in the schema,
-- the query does not validate.
fragmentSpreadTypeExistenceRule :: forall m. Rule m
fragmentSpreadTypeExistenceRule = SelectionRule $ \case
    FragmentSpreadSelection fragmentSelection
        | FragmentSpread fragmentName _ location <- fragmentSelection -> do
            ast' <- asks ast
            let target = find (isSpreadTarget fragmentName) ast'
            typeCondition <- lift $ maybeToSeq $ target >>= extractTypeCondition
            types' <- asks types
            case HashMap.lookup typeCondition types' of
                Nothing -> pure $ Error
                    { message = spreadError fragmentName typeCondition
                    , locations = [location]
                    , path = []
                    }
                Just _ -> lift mempty
    InlineFragmentSelection fragmentSelection
        | InlineFragment maybeType _ _ location <- fragmentSelection
        , Just typeCondition <- maybeType -> do
            types' <- asks types
            case HashMap.lookup typeCondition types' of
                Nothing -> pure $ Error
                    { message = inlineError typeCondition
                    , locations = [location]
                    , path = []
                    }
                Just _ -> lift mempty
    _ -> lift mempty
  where
    extractTypeCondition (viewFragment -> Just fragmentDefinition) =
        let FragmentDefinition _ typeCondition _ _ _ = fragmentDefinition
         in Just typeCondition
    extractTypeCondition _ = Nothing
    spreadError fragmentName typeCondition = concat
        [ "Fragment \""
        , Text.unpack fragmentName
        , "\" is specified on type \""
        , Text.unpack typeCondition
        , "\" which doesn't exist in the schema."
        ]
    inlineError typeCondition = concat
        [ "Inline fragment is specified on type \""
        , Text.unpack typeCondition
        , "\" which doesn't exist in the schema."
        ]

maybeToSeq :: forall a. Maybe a -> Seq a
maybeToSeq (Just x) = pure x
maybeToSeq Nothing = mempty

-- | Fragments can only be declared on unions, interfaces, and objects. They are
-- invalid on scalars. They can only be applied on non‐leaf fields. This rule
-- applies to both inline and named fragments.
fragmentsOnCompositeTypesRule :: forall m. Rule m
fragmentsOnCompositeTypesRule = FragmentRule definitionRule inlineRule
  where
    inlineRule (InlineFragment (Just typeCondition) _ _ location) =
        check typeCondition location
    inlineRule _ = lift mempty
    definitionRule (FragmentDefinition _ typeCondition _ _ location) =
        check typeCondition location
    check typeCondition location = do
        types' <- asks types
        -- Skip unknown types, they are checked by another rule.
        _ <- lift $ maybeToSeq $ HashMap.lookup typeCondition types'
        case lookupTypeCondition typeCondition types' of
            Nothing -> pure $ Error
                { message = errorMessage typeCondition
                , locations = [location]
                , path = []
                }
            Just _ -> lift mempty
    errorMessage typeCondition = concat
        [ "Fragment cannot condition on non composite type \""
        , Text.unpack typeCondition,
        "\"."
        ]

-- | Defined fragments must be used within a document.
noUnusedFragmentsRule :: forall m. Rule m
noUnusedFragmentsRule = FragmentDefinitionRule $ \fragment ->
    asks ast >>= findSpreadByName fragment
  where
    findSpreadByName (FragmentDefinition fragName _ _ _ location) definitions
        | foldr (go fragName) False definitions = lift mempty
        | otherwise = pure $ Error
            { message = errorMessage fragName
            , locations = [location]
            , path = []
            }
    errorMessage fragName = concat
        [ "Fragment \""
        , Text.unpack fragName
        , "\" is never used."
        ]
    go fragName (viewOperation -> Just operation) accumulator
        | SelectionSet selections _ <- operation =
            evaluateSelections fragName accumulator selections
        | OperationDefinition _ _ _ _ selections _ <- operation =
            evaluateSelections fragName accumulator selections
    go fragName (viewFragment -> Just fragment) accumulator
        | FragmentDefinition _ _ _ selections _ <- fragment =
            evaluateSelections fragName accumulator selections
    go _ _ _ = False
    evaluateSelection fragName selection accumulator
        | FragmentSpreadSelection spreadSelection <- selection
        , FragmentSpread spreadName _ _ <- spreadSelection
        , spreadName == fragName = True
        | FieldSelection fieldSelection <- selection
        , Field _ _ _ _ selections _ <- fieldSelection =
            evaluateSelections fragName accumulator selections
        | InlineFragmentSelection inlineSelection <- selection
        , InlineFragment _ _ selections _ <- inlineSelection =
            evaluateSelections fragName accumulator selections
        | otherwise = accumulator || False
    evaluateSelections :: Foldable t => Name -> Bool -> t Selection -> Bool
    evaluateSelections fragName accumulator selections =
        foldr (evaluateSelection fragName) accumulator selections

-- | The graph of fragment spreads must not form any cycles including spreading
-- itself. Otherwise an operation could infinitely spread or infinitely execute
-- on cycles in the underlying data.
noFragmentCyclesRule :: forall m. Rule m
noFragmentCyclesRule = FragmentDefinitionRule $ \case
    FragmentDefinition fragmentName _ _ selections location -> do
        state <- evalStateT (collectFields selections)
            (0, fragmentName)
        let spreadPath = fst <$> sortBy (comparing snd) (HashMap.toList state)
        case reverse spreadPath of
            x : _ | x == fragmentName -> pure $ Error
                { message = concat
                    [ "Cannot spread fragment \""
                    , Text.unpack fragmentName
                    , "\" within itself (via "
                    , Text.unpack $ Text.intercalate " -> " $ fragmentName : spreadPath
                    , ")."
                    ]
                , locations = [location]
                , path = []
                }
            _ -> lift mempty
  where
    collectFields :: Traversable t
        => forall m
        . t Selection
        -> StateT (Int, Name) (ReaderT (Validation m) Seq) (HashMap Name Int)
    collectFields selectionSet = foldM forEach HashMap.empty selectionSet
    forEach accumulator = \case
        FieldSelection fieldSelection -> forField accumulator fieldSelection
        InlineFragmentSelection fragmentSelection ->
            forInline accumulator fragmentSelection
        FragmentSpreadSelection fragmentSelection ->
            forSpread accumulator fragmentSelection
    forSpread accumulator (FragmentSpread fragmentName _ _) = do
        firstFragmentName <- gets snd
        modify $ first (+ 1)
        lastIndex <- gets fst
        let newAccumulator = HashMap.insert fragmentName lastIndex accumulator
        let inVisitetFragment =HashMap.member fragmentName accumulator
        if fragmentName == firstFragmentName || inVisitetFragment
            then pure newAccumulator
            else collectFromSpread fragmentName newAccumulator
    forInline accumulator (InlineFragment _ _ selections _) =
        (accumulator <>) <$> collectFields selections
    forField accumulator (Field _ _ _ _ selections _) =
        (accumulator <>) <$> collectFields selections
    findFragmentDefinition n (ExecutableDefinition executableDefinition) Nothing
        | DefinitionFragment fragmentDefinition <- executableDefinition
        , FragmentDefinition fragmentName _ _ _ _ <- fragmentDefinition
        , fragmentName == n = Just fragmentDefinition
    findFragmentDefinition _ _ accumulator = accumulator
    collectFromSpread _fragmentName accumulator = do
        ast' <- lift $ asks ast
        case foldr (findFragmentDefinition _fragmentName) Nothing ast' of
            Nothing -> pure accumulator
            Just (FragmentDefinition _ _ _ selections _) ->
                (accumulator <>) <$> collectFields selections
