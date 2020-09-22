{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module contains default rules defined in the GraphQL specification.
module Language.GraphQL.Validate.Rules
    ( executableDefinitionsRule
    , fragmentsOnCompositeTypesRule
    , fragmentSpreadTargetDefinedRule
    , fragmentSpreadTypeExistenceRule
    , loneAnonymousOperationRule
    , noFragmentCyclesRule
    , noUndefinedVariablesRule
    , noUnusedFragmentsRule
    , noUnusedVariablesRule
    , singleFieldSubscriptionsRule
    , specifiedRules
    , uniqueArgumentNamesRule
    , uniqueDirectiveNamesRule
    , uniqueFragmentNamesRule
    , uniqueOperationNamesRule
    , uniqueVariableNamesRule
    , variablesAreInputTypesRule
    ) where

import Control.Monad ((>=>), foldM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..), asks, mapReaderT)
import Control.Monad.Trans.State (StateT, evalStateT, gets, modify)
import Data.Bifunctor (first)
import Data.Foldable (find, toList)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (groupBy, sortBy, sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Language.GraphQL.AST.Document
import Language.GraphQL.Type.Internal
import qualified Language.GraphQL.Type.Schema as Schema
import Language.GraphQL.Validate.Validation

-- Local help type that contains a hash set to track visited fragments.
type ValidationState m a = StateT (HashSet Name) (ReaderT (Validation m) Seq) a

-- | Default rules given in the specification.
specifiedRules :: forall m. [Rule m]
specifiedRules =
    -- Documents.
    [ executableDefinitionsRule
    -- Operations.
    , singleFieldSubscriptionsRule
    , loneAnonymousOperationRule
    , uniqueOperationNamesRule
    -- Arguments.
    , uniqueArgumentNamesRule
    -- Fragments.
    , uniqueFragmentNamesRule
    , fragmentSpreadTypeExistenceRule
    , fragmentsOnCompositeTypesRule
    , noUnusedFragmentsRule
    , fragmentSpreadTargetDefinedRule
    , noFragmentCyclesRule
    -- Directives.
    , uniqueDirectiveNamesRule
    -- Variables.
    , uniqueVariableNamesRule
    , variablesAreInputTypesRule
    , noUndefinedVariablesRule
    , noUnusedVariablesRule
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
                    }
                | otherwise -> pure $ Error
                    { message = errorMessage
                    , locations = [location]
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
    skip (Directive "skip" [Argument "if" (Node argumentValue _) _] _) =
        Boolean True == argumentValue
    skip (Directive "include" [Argument "if" (Node argumentValue _) _] _) =
        Boolean False == argumentValue
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
                }
            Just _ -> lift mempty
    errorMessage typeCondition = concat
        [ "Fragment cannot condition on non composite type \""
        , Text.unpack typeCondition,
        "\"."
        ]

-- | Defined fragments must be used within a document.
noUnusedFragmentsRule :: forall m. Rule m
noUnusedFragmentsRule = FragmentDefinitionRule $ \fragment -> do
    let FragmentDefinition fragmentName _ _ _ location = fragment
     in mapReaderT (checkFragmentName fragmentName location)
        $ asks ast
        >>= flip evalStateT HashSet.empty
        . filterSelections evaluateSelection
        . foldMap definitionSelections
  where
    checkFragmentName fragmentName location elements
        | fragmentName `elem` elements = mempty
        | otherwise = pure $ makeError fragmentName location
    makeError fragName location = Error
        { message = errorMessage fragName
        , locations = [location]
        }
    errorMessage fragName = concat
        [ "Fragment \""
        , Text.unpack fragName
        , "\" is never used."
        ]
    evaluateSelection selection
        | FragmentSpreadSelection spreadSelection <- selection
        , FragmentSpread spreadName _ _ <- spreadSelection =
            lift $ pure spreadName
    evaluateSelection _ = lift $ lift mempty

definitionSelections :: Definition -> SelectionSetOpt
definitionSelections (viewOperation -> Just operation)
    | OperationDefinition _ _ _ _ selections _ <- operation = toList selections
    | SelectionSet selections _ <- operation = toList selections
definitionSelections (viewFragment -> Just fragment)
    | FragmentDefinition _ _ _ selections _ <- fragment = toList selections
definitionSelections _ = []

filterSelections :: Foldable t
    => forall a m
    . (Selection -> ValidationState m a)
    -> t Selection
    -> ValidationState m a
filterSelections applyFilter selections
    = (lift . lift) (Seq.fromList $ foldr evaluateSelection mempty selections)
    >>= applyFilter
  where
    evaluateSelection selection accumulator
        | FragmentSpreadSelection{} <- selection = selection : accumulator
        | FieldSelection fieldSelection <- selection
        , Field _ _ _ _ subselections _ <- fieldSelection =
            selection : foldr evaluateSelection accumulator subselections
        | InlineFragmentSelection inlineSelection <- selection
        , InlineFragment _ _ subselections _ <- inlineSelection =
            selection : foldr evaluateSelection accumulator subselections

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
                }
            _ -> lift mempty
  where
    collectFields :: Traversable t
        => t Selection
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
        let inVisitetFragment = HashMap.member fragmentName accumulator
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

-- | Fields and directives treat arguments as a mapping of argument name to
-- value. More than one argument with the same name in an argument set is
-- ambiguous and invalid.
uniqueArgumentNamesRule :: forall m. Rule m
uniqueArgumentNamesRule = ArgumentsRule fieldRule directiveRule
  where
    fieldRule (Field _ _ arguments _ _ _) =
        filterDuplicates extract "argument" arguments
    directiveRule (Directive _ arguments _) =
        filterDuplicates extract "argument" arguments
    extract (Argument argumentName _ location) = (argumentName, location)

-- | Directives are used to describe some metadata or behavioral change on the
-- definition they apply to. When more than one directive of the same name is
-- used, the expected metadata or behavior becomes ambiguous, therefore only one
-- of each directive is allowed per location.
uniqueDirectiveNamesRule :: forall m. Rule m
uniqueDirectiveNamesRule = DirectivesRule
    $ filterDuplicates extract "directive"
  where
    extract (Directive directiveName _ location) = (directiveName, location)

filterDuplicates :: (a -> (Text, Location)) -> String -> [a] -> RuleT m
filterDuplicates extract nodeType = lift
    . Seq.fromList
    . fmap makeError
    . filter ((> 1) . length)
    . groupBy equalByName
    . sortOn getName
  where
    getName = fst . extract
    equalByName lhs rhs = getName lhs == getName rhs
    makeError directives = Error
        { message = makeMessage $ head directives
        , locations = snd . extract <$> directives
        }
    makeMessage directive = concat
        [ "There can be only one "
        , nodeType
        , " named \""
        , Text.unpack $ fst $ extract directive
        , "\"."
        ]

-- | If any operation defines more than one variable with the same name, it is
-- ambiguous and invalid. It is invalid even if the type of the duplicate
-- variable is the same.
uniqueVariableNamesRule :: forall m. Rule m
uniqueVariableNamesRule = VariablesRule
    $ filterDuplicates extract "variable"
  where
    extract (VariableDefinition variableName _ _ location) =
        (variableName, location)

-- | Variables can only be input types. Objects, unions and interfaces cannot be
-- used as inputs.
variablesAreInputTypesRule :: forall m. Rule m
variablesAreInputTypesRule = VariablesRule
    $ (traverse check . Seq.fromList) >=> lift
  where
    check (VariableDefinition name typeName _ location)
        = asks types
        >>= lift
        . maybe (makeError name typeName location) (const mempty)
        . lookupInputType typeName
    makeError name typeName location = pure $ Error
        { message = concat
            [ "Variable \"$"
            , Text.unpack name
            , "\" cannot be non-input type \""
            , Text.unpack $ getTypeName typeName
            , "\"."
            ]
        , locations = [location]
        }
    getTypeName (TypeNamed name) = name
    getTypeName (TypeList name) = getTypeName name
    getTypeName (TypeNonNull (NonNullTypeNamed nonNull)) = nonNull
    getTypeName (TypeNonNull (NonNullTypeList nonNull)) = getTypeName nonNull

-- | Variables are scoped on a per‐operation basis. That means that any variable
-- used within the context of an operation must be defined at the top level of
-- that operation.
noUndefinedVariablesRule :: forall m. Rule m
noUndefinedVariablesRule =
    variableUsageDifference (flip HashMap.difference) errorMessage
  where
    errorMessage Nothing variableName = concat
        [ "Variable \"$"
        , Text.unpack variableName
        , "\" is not defined."
        ]
    errorMessage (Just operationName) variableName = concat
        [ "Variable \"$"
        , Text.unpack variableName
        , "\" is not defined by operation \""
        , Text.unpack operationName
        , "\"."
        ]

variableUsageDifference :: forall m
    . (HashMap Name [Location] -> HashMap Name [Location] -> HashMap Name [Location])
    -> (Maybe Name -> Name -> String)
    -> Rule m
variableUsageDifference difference errorMessage = OperationDefinitionRule $ \case
    SelectionSet _ _ -> lift mempty
    OperationDefinition _ operationName variables _ selections _ ->
        let variableNames = HashMap.fromList $ getVariableName <$> variables
         in mapReaderT (readerMapper operationName variableNames)
            $ flip evalStateT HashSet.empty
            $ filterSelections'
            $ toList selections
  where
    readerMapper operationName variableNames' = Seq.fromList
        . fmap (makeError operationName)
        . HashMap.toList
        . difference variableNames'
        . HashMap.fromListWith (++)
        . toList
    getVariableName (VariableDefinition variableName _ _ location) =
        (variableName, [location])
    filterSelections' :: Foldable t
        => t Selection
        -> ValidationState m (Name, [Location])
    filterSelections' = filterSelections variableFilter
    variableFilter :: Selection -> ValidationState m (Name, [Location])
    variableFilter (InlineFragmentSelection inline)
        | InlineFragment _ directives _ _ <- inline =
            lift $ lift $ mapDirectives directives
    variableFilter (FieldSelection fieldSelection)
        | Field _ _ arguments directives _ _ <- fieldSelection =
            lift $ lift $ mapArguments arguments <> mapDirectives directives
    variableFilter (FragmentSpreadSelection spread)
        | FragmentSpread fragmentName _ _ <- spread = do
            definitions <- lift $ asks ast
            visited <- gets (HashSet.member fragmentName)
            modify (HashSet.insert fragmentName)
            case find (isSpreadTarget fragmentName) definitions of
                Just (viewFragment -> Just fragmentDefinition)
                    | not visited -> diveIntoSpread fragmentDefinition
                _ -> lift $ lift mempty
    diveIntoSpread (FragmentDefinition _ _ directives selections _)
        = filterSelections' selections
        >>= lift . mapReaderT (<> mapDirectives directives) . pure
    findDirectiveVariables (Directive _ arguments _) = mapArguments arguments
    mapArguments = Seq.fromList . mapMaybe findArgumentVariables
    mapDirectives = foldMap findDirectiveVariables
    findArgumentVariables (Argument _ (Node (Variable value) location) _) =
        Just (value, [location])
    findArgumentVariables _ = Nothing
    makeError operationName (variableName, locations') = Error
        { message = errorMessage operationName variableName
        , locations = locations'
        }

-- | All variables defined by an operation must be used in that operation or a
-- fragment transitively included by that operation. Unused variables cause a
-- validation error.
noUnusedVariablesRule :: forall m. Rule m
noUnusedVariablesRule = variableUsageDifference HashMap.difference errorMessage
  where
    errorMessage Nothing variableName = concat
        [ "Variable \"$"
        , Text.unpack variableName
        , "\" is never used."
        ]
    errorMessage (Just operationName) variableName = concat
        [ "Variable \"$"
        , Text.unpack variableName
        , "\" is never used in operation \""
        , Text.unpack operationName
        , "\"."
        ]
