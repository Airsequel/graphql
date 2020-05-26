{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | After the document is parsed, before getting executed, the AST is
-- transformed into a similar, simpler AST. Performed transformations include:
--
--   * Replacing variables with their values.
--   * Inlining fragments. Some fragments can be completely eliminated and
--   replaced by the selection set they represent. Invalid (recursive and
--   non-existing) fragments are skipped. The most fragments are inlined, so the
--   executor doesn't have to perform additional lookups later.
--
-- This module is also responsible for smaller rewrites that touch only parts of
-- the original AST.
module Language.GraphQL.Execute.Transform
    ( Document(..)
    , QueryError(..)
    , document
    , queryError
    ) where

import Control.Monad (foldM, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (State, evalStateT, gets, modify)
import Data.Foldable (find)
import Data.Functor.Identity (Identity(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Sequence (Seq, (<|), (><))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.GraphQL.AST as Full
import qualified Language.GraphQL.AST.Core as Core
import Language.GraphQL.Execute.Coerce
import qualified Language.GraphQL.Schema as Schema
import qualified Language.GraphQL.Type.Directive as Directive
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema

-- | Associates a fragment name with a list of 'Core.Field's.
data Replacement m = Replacement
    { fragments :: HashMap Core.Name Core.Fragment
    , fragmentDefinitions :: FragmentDefinitions
    , variableValues :: Schema.Subs
    , types :: HashMap Full.Name (Type m)
    }

type FragmentDefinitions = HashMap Full.Name Full.FragmentDefinition

-- | Contains the operation to be executed along with its root type.
data Document m = Document (Out.ObjectType m) Core.Operation

data OperationDefinition = OperationDefinition
    Full.OperationType
    (Maybe Full.Name)
    [Full.VariableDefinition]
    [Full.Directive]
    Full.SelectionSet

-- | Query error types.
data QueryError
    = OperationNotFound Text
    | OperationNameRequired
    | CoercionError
    | TransformationError
    | EmptyDocument
    | UnsupportedRootOperation

queryError :: QueryError -> Text
queryError (OperationNotFound operationName) = Text.unwords
    ["Operation", operationName, "couldn't be found in the document."]
queryError OperationNameRequired = "Missing operation name."
queryError CoercionError = "Coercion error."
queryError TransformationError = "Schema transformation error."
queryError EmptyDocument =
    "The document doesn't contain any executable operations."
queryError UnsupportedRootOperation =
    "Root operation type couldn't be found in the schema."

getOperation
    :: Maybe Full.Name
    -> NonEmpty OperationDefinition
    -> Either QueryError OperationDefinition
getOperation Nothing (operation' :| []) = pure operation'
getOperation Nothing _ = Left OperationNameRequired
getOperation (Just operationName) operations
    | Just operation' <- find matchingName operations = pure operation'
    | otherwise = Left $ OperationNotFound operationName
  where
    matchingName (OperationDefinition _ name _ _ _) =
        name == Just operationName

lookupInputType
    :: Full.Type
    -> HashMap.HashMap Full.Name (Type m)
    -> Maybe In.Type
lookupInputType (Full.TypeNamed name) types =
    case HashMap.lookup name types of
        Just (ScalarType scalarType) ->
            Just $ In.NamedScalarType scalarType
        Just (EnumType enumType) ->
            Just $ In.NamedEnumType enumType
        Just (InputObjectType objectType) ->
            Just $ In.NamedInputObjectType objectType
        _ -> Nothing
lookupInputType (Full.TypeList list) types
    = In.ListType
    <$> lookupInputType list types
lookupInputType (Full.TypeNonNull (Full.NonNullTypeNamed nonNull)) types  =
    case HashMap.lookup nonNull types of
        Just (ScalarType scalarType) ->
            Just $ In.NonNullScalarType scalarType
        Just (EnumType enumType) ->
            Just $ In.NonNullEnumType enumType
        Just (InputObjectType objectType) ->
            Just $ In.NonNullInputObjectType objectType
        _ -> Nothing
lookupInputType (Full.TypeNonNull (Full.NonNullTypeList nonNull)) types
    = In.NonNullListType
    <$> lookupInputType nonNull types

coerceVariableValues :: VariableValue a
    => forall m
    . HashMap Full.Name (Type m)
    -> OperationDefinition
    -> HashMap.HashMap Full.Name a
    -> Either QueryError Schema.Subs
coerceVariableValues types operationDefinition variableValues' =
    let OperationDefinition _ _ variableDefinitions _ _ = operationDefinition
     in maybe (Left CoercionError) Right
        $ foldr coerceValue (Just HashMap.empty) variableDefinitions
  where
    coerceValue variableDefinition coercedValues = do
        let Full.VariableDefinition variableName variableTypeName defaultValue =
                variableDefinition
        let defaultValue' = constValue <$> defaultValue
        let value' = HashMap.lookup variableName variableValues'

        variableType <- lookupInputType variableTypeName types
        HashMap.insert variableName
            <$> choose value' defaultValue' variableType
            <*> coercedValues
    choose Nothing defaultValue variableType
        | Just _ <- defaultValue = defaultValue
        | not (In.isNonNullType variableType) = Just In.Null
    choose (Just value') _ variableType
        | Just coercedValue <- coerceVariableValue variableType value'
        , not (In.isNonNullType variableType) || coercedValue /= In.Null =
            Just coercedValue
    choose _ _ _ = Nothing

constValue :: Full.ConstValue -> In.Value
constValue (Full.ConstInt i) = In.Int i
constValue (Full.ConstFloat f) = In.Float f
constValue (Full.ConstString x) = In.String x
constValue (Full.ConstBoolean b) = In.Boolean b
constValue Full.ConstNull = In.Null
constValue (Full.ConstEnum e) = In.Enum e
constValue (Full.ConstList l) = In.List $ constValue <$> l
constValue (Full.ConstObject o) =
    In.Object $ HashMap.fromList $ constObjectField <$> o
  where
    constObjectField (Full.ObjectField key value') = (key, constValue value')

-- | Rewrites the original syntax tree into an intermediate representation used
-- for query execution.
document :: VariableValue a
    => forall m
    . Schema m
    -> Maybe Full.Name
    -> HashMap Full.Name a
    -> Full.Document
    -> Either QueryError (Document m)
document schema operationName subs ast = do
    let referencedTypes = collectReferencedTypes schema

    (operations, fragmentTable) <- defragment ast
    chosenOperation <- getOperation operationName operations
    coercedValues <- coerceVariableValues referencedTypes chosenOperation subs

    let replacement = Replacement
            { fragments = HashMap.empty
            , fragmentDefinitions = fragmentTable
            , variableValues = coercedValues
            , types = referencedTypes
            }
    case chosenOperation of
        OperationDefinition Full.Query _ _ _ _ ->
            pure $ Document (query schema)
                $ operation (query schema) chosenOperation replacement
        OperationDefinition Full.Mutation _ _ _ _
            | Just mutationType <- mutation schema ->
                pure $ Document mutationType
                    $ operation mutationType chosenOperation replacement
        _ -> Left UnsupportedRootOperation

defragment
    :: Full.Document
    -> Either QueryError (NonEmpty OperationDefinition, FragmentDefinitions)
defragment ast =
    let (operations, fragmentTable) = foldr defragment' ([], HashMap.empty) ast
        nonEmptyOperations = NonEmpty.nonEmpty operations
        emptyDocument = Left EmptyDocument
     in (, fragmentTable) <$> maybe emptyDocument Right nonEmptyOperations
  where
    defragment' definition (operations, fragments')
        | (Full.ExecutableDefinition executable) <- definition
        , (Full.DefinitionOperation operation') <- executable =
            (transform operation' : operations, fragments')
        | (Full.ExecutableDefinition executable) <- definition
        , (Full.DefinitionFragment fragment) <- executable
        , (Full.FragmentDefinition name _ _ _) <- fragment =
            (operations, HashMap.insert name fragment fragments')
    defragment' _ acc = acc
    transform = \case
        Full.OperationDefinition type' name variables directives' selections ->
            OperationDefinition type' name variables directives' selections
        Full.SelectionSet selectionSet ->
            OperationDefinition Full.Query Nothing mempty mempty selectionSet

-- * Operation

operation :: forall m
    . Out.ObjectType m
    -> OperationDefinition
    -> Replacement m
    -> Core.Operation
operation rootType operationDefinition replacement
    = runIdentity
    $ evalStateT (collectFragments rootType >> transform operationDefinition) replacement
  where
    transform (OperationDefinition Full.Query name _ _ sels) =
        Core.Query name <$> appendSelection sels rootType
    transform (OperationDefinition Full.Mutation name _ _ sels) =
        Core.Mutation name <$> appendSelection sels rootType

-- * Selection

selection :: forall m
    . Full.Selection
    -> Out.ObjectType m
    -> State (Replacement m) (Either (Seq Core.Selection) Core.Selection)
selection (Full.Field alias name arguments' directives' selections) objectType =
    maybe (Left mempty) (Right . Core.SelectionField) <$> do
        fieldArguments <- arguments arguments'
        fieldSelections <- appendSelection selections objectType
        fieldDirectives <- Directive.selection <$> directives directives'
        let field' = Core.Field alias name fieldArguments fieldSelections
        pure $ field' <$ fieldDirectives
selection (Full.FragmentSpread name directives') objectType =
    maybe (Left mempty) (Right . Core.SelectionFragment) <$> do
        spreadDirectives <- Directive.selection <$> directives directives'
        fragments' <- gets fragments

        fragmentDefinitions' <- gets fragmentDefinitions
        case HashMap.lookup name fragments' of
            Just definition -> lift $ pure $ definition <$ spreadDirectives
            Nothing -> case HashMap.lookup name fragmentDefinitions' of
                         Just definition -> do
                             fragment <- fragmentDefinition definition objectType
                             lift $ pure $ fragment <$ spreadDirectives
                         Nothing -> lift $ pure  Nothing
selection (Full.InlineFragment type' directives' selections) objectType = do
    fragmentDirectives <- Directive.selection <$> directives directives'
    case fragmentDirectives of
        Nothing -> pure $ Left mempty
        _ -> do
            fragmentSelectionSet <- appendSelection selections objectType
            pure $ maybe Left selectionFragment type' fragmentSelectionSet
  where
    selectionFragment typeName = Right
        . Core.SelectionFragment
        . Core.Fragment typeName

appendSelection :: Traversable t
    => forall m
    . t Full.Selection
    -> Out.ObjectType m
    -> State (Replacement m) (Seq Core.Selection)
appendSelection selectionSet objectType = foldM go mempty selectionSet
  where
    go acc sel = append acc <$> selection sel objectType
    append acc (Left list) = list >< acc
    append acc (Right one) = one <| acc

directives :: forall m
    . [Full.Directive]
    -> State (Replacement m) [Core.Directive]
directives = traverse directive
  where
    directive (Full.Directive directiveName directiveArguments) =
        Core.Directive directiveName <$> arguments directiveArguments

-- * Fragment replacement

-- | Extract fragment definitions into a single 'HashMap'.
collectFragments :: forall m. Out.ObjectType m -> State (Replacement m) ()
collectFragments objectType = do
    fragDefs <- gets fragmentDefinitions
    let nextValue = head $ HashMap.elems fragDefs
    unless (HashMap.null fragDefs) $ do
        _ <- fragmentDefinition nextValue objectType
        collectFragments objectType

fragmentDefinition :: forall m
    . Full.FragmentDefinition
    -> Out.ObjectType m
    -> State (Replacement m) Core.Fragment
fragmentDefinition (Full.FragmentDefinition name type' _ selections) objectType = do
    modify deleteFragmentDefinition
    fragmentSelection <- appendSelection selections objectType
    let newValue = Core.Fragment type' fragmentSelection
    modify $ insertFragment newValue
    lift $ pure newValue
  where
    deleteFragmentDefinition replacement@Replacement{..} =
        let newDefinitions = HashMap.delete name fragmentDefinitions
         in replacement{ fragmentDefinitions = newDefinitions }
    insertFragment newValue replacement@Replacement{..} =
        let newFragments = HashMap.insert name newValue fragments
         in replacement{ fragments = newFragments }

arguments :: forall m. [Full.Argument] -> State (Replacement m) Core.Arguments
arguments = fmap Core.Arguments . foldM go HashMap.empty
  where
    go arguments' (Full.Argument name value') = do
        substitutedValue <- value value'
        return $ HashMap.insert name substitutedValue arguments'

value :: forall m. Full.Value -> State (Replacement m) In.Value
value (Full.Variable name) =
    gets $ fromMaybe In.Null . HashMap.lookup name . variableValues
value (Full.Int i) = pure $ In.Int i
value (Full.Float f) = pure $ In.Float f
value (Full.String x) = pure $ In.String x
value (Full.Boolean b) = pure $ In.Boolean b
value Full.Null = pure   In.Null
value (Full.Enum e) = pure $ In.Enum e
value (Full.List l) = In.List <$> traverse value l
value (Full.Object o) =
    In.Object . HashMap.fromList <$> traverse objectField o

objectField :: forall m
    . Full.ObjectField Full.Value
    -> State (Replacement m) (Core.Name, In.Value)
objectField (Full.ObjectField name value') = (name,) <$> value value'
