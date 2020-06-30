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
--   * Evaluating directives (@\@include@ and @\@skip@).
--
-- This module is also responsible for smaller rewrites that touch only parts of
-- the original AST.
module Language.GraphQL.Execute.Transform
    ( Document(..)
    , Field(..)
    , Fragment(..)
    , Input(..)
    , Operation(..)
    , QueryError(..)
    , Selection(..)
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
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Sequence (Seq, (<|), (><))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.GraphQL.AST as Full
import Language.GraphQL.AST (Name)
import qualified Language.GraphQL.Execute.Coerce as Coerce
import Language.GraphQL.Type.Directive (Directive(..))
import qualified Language.GraphQL.Type.Directive as Directive
import qualified Language.GraphQL.Type as Type
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema

-- | Associates a fragment name with a list of 'Field's.
data Replacement m = Replacement
    { fragments :: HashMap Full.Name (Fragment m)
    , fragmentDefinitions :: FragmentDefinitions
    , variableValues :: Type.Subs
    , types :: HashMap Full.Name (Type m)
    }

type FragmentDefinitions = HashMap Full.Name Full.FragmentDefinition

-- | Represents fragments and inline fragments.
data Fragment m
    = Fragment (CompositeType m) (Seq (Selection m))

-- | Single selection element.
data Selection m
    = SelectionFragment (Fragment m)
    | SelectionField (Field m)

-- | GraphQL has 3 operation types: queries, mutations and subscribtions.
--
-- Currently only queries and mutations are supported.
data Operation m
    = Query (Maybe Text) (Seq (Selection m))
    | Mutation (Maybe Text) (Seq (Selection m))

-- | Single GraphQL field.
data Field m = Field
    (Maybe Full.Name) Full.Name (HashMap Full.Name Input) (Seq (Selection m))

-- | Contains the operation to be executed along with its root type.
data Document m = Document
    (HashMap Full.Name (Type m)) (Out.ObjectType m) (Operation m)

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

data Input
    = Int Int32
    | Float Double
    | String Text
    | Boolean Bool
    | Null
    | Enum Name
    | List [Type.Value]
    | Object (HashMap Name Input)
    | Variable Type.Value
    deriving (Eq, Show)

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

coerceVariableValues :: Coerce.VariableValue a
    => forall m
    . HashMap Full.Name (Type m)
    -> OperationDefinition
    -> HashMap.HashMap Full.Name a
    -> Either QueryError Type.Subs
coerceVariableValues types operationDefinition variableValues =
    let OperationDefinition _ _ variableDefinitions _ _ = operationDefinition
     in maybe (Left CoercionError) Right
        $ foldr forEach (Just HashMap.empty) variableDefinitions
  where
    forEach variableDefinition coercedValues = do
        let Full.VariableDefinition variableName variableTypeName defaultValue =
                variableDefinition
        let defaultValue' = constValue <$> defaultValue
        variableType <- lookupInputType variableTypeName types

        Coerce.matchFieldValues
            coerceVariableValue'
            variableValues
            variableName
            variableType
            defaultValue'
            coercedValues
    coerceVariableValue' variableType value'
        = Coerce.coerceVariableValue variableType value'
        >>= Coerce.coerceInputLiteral variableType

constValue :: Full.ConstValue -> Type.Value
constValue (Full.ConstInt i) = Type.Int i
constValue (Full.ConstFloat f) = Type.Float f
constValue (Full.ConstString x) = Type.String x
constValue (Full.ConstBoolean b) = Type.Boolean b
constValue Full.ConstNull = Type.Null
constValue (Full.ConstEnum e) = Type.Enum e
constValue (Full.ConstList l) = Type.List $ constValue <$> l
constValue (Full.ConstObject o) =
    Type.Object $ HashMap.fromList $ constObjectField <$> o
  where
    constObjectField (Full.ObjectField key value') = (key, constValue value')

-- | Rewrites the original syntax tree into an intermediate representation used
-- for query execution.
document :: Coerce.VariableValue a
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
            pure $ Document referencedTypes (query schema)
                $ operation chosenOperation replacement
        OperationDefinition Full.Mutation _ _ _ _
            | Just mutationType <- mutation schema ->
                pure $ Document referencedTypes mutationType
                    $ operation chosenOperation replacement
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

operation :: OperationDefinition -> Replacement m -> Operation m
operation operationDefinition replacement
    = runIdentity
    $ evalStateT (collectFragments >> transform operationDefinition) replacement
  where
    transform (OperationDefinition Full.Query name _ _ sels) =
        Query name <$> appendSelection sels
    transform (OperationDefinition Full.Mutation name _ _ sels) =
        Mutation name <$> appendSelection sels

-- * Selection

selection
    :: Full.Selection
    ->  State (Replacement m) (Either (Seq (Selection m)) (Selection m))
selection (Full.Field alias name arguments' directives' selections) =
    maybe (Left mempty) (Right . SelectionField) <$> do
        fieldArguments <- foldM go HashMap.empty arguments'
        fieldSelections <- appendSelection selections
        fieldDirectives <- Directive.selection <$> directives directives'
        let field' = Field alias name fieldArguments fieldSelections
        pure $ field' <$ fieldDirectives
  where
    go arguments (Full.Argument name' value') =
        inputField arguments name' value'

selection (Full.FragmentSpread name directives') =
    maybe (Left mempty) (Right . SelectionFragment) <$> do
        spreadDirectives <- Directive.selection <$> directives directives'
        fragments' <- gets fragments

        fragmentDefinitions' <- gets fragmentDefinitions
        case HashMap.lookup name fragments' of
            Just definition -> lift $ pure $ definition <$ spreadDirectives
            Nothing
                | Just definition <- HashMap.lookup name fragmentDefinitions' -> do
                    fragDef <- fragmentDefinition definition
                    case fragDef of
                        Just fragment -> lift $ pure $ fragment <$ spreadDirectives
                        _ -> lift $ pure  Nothing
                | otherwise -> lift $ pure  Nothing
selection (Full.InlineFragment type' directives' selections) = do
    fragmentDirectives <- Directive.selection <$> directives directives'
    case fragmentDirectives of
        Nothing -> pure $ Left mempty
        _ -> do
            fragmentSelectionSet <- appendSelection selections

            case type' of
                Nothing -> pure $ Left fragmentSelectionSet
                Just typeName -> do
                    typeCondition' <- lookupTypeCondition typeName
                    case typeCondition' of
                        Just typeCondition -> pure $
                            selectionFragment typeCondition fragmentSelectionSet
                        Nothing -> pure $ Left mempty
  where
    selectionFragment typeName = Right
        . SelectionFragment
        . Fragment typeName

appendSelection :: Traversable t
    => t Full.Selection
    ->  State (Replacement m) (Seq (Selection m))
appendSelection = foldM go mempty
  where
    go acc sel = append acc <$> selection sel
    append acc (Left list) = list >< acc
    append acc (Right one) = one <| acc

directives :: [Full.Directive] ->  State (Replacement m) [Directive]
directives = traverse directive
  where
    directive (Full.Directive directiveName directiveArguments)
        = Directive directiveName . Type.Arguments
        <$> foldM go HashMap.empty directiveArguments
    go arguments (Full.Argument name value') = do
        substitutedValue <- value value'
        return $ HashMap.insert name substitutedValue arguments

-- * Fragment replacement

-- | Extract fragment definitions into a single 'HashMap'.
collectFragments ::  State (Replacement m) ()
collectFragments = do
    fragDefs <- gets fragmentDefinitions
    let nextValue = head $ HashMap.elems fragDefs
    unless (HashMap.null fragDefs) $ do
        _ <- fragmentDefinition nextValue
        collectFragments

lookupTypeCondition :: Full.Name -> State (Replacement m) (Maybe (CompositeType m))
lookupTypeCondition type' = do
    types' <- gets types
    case HashMap.lookup type' types' of
        Just (ObjectType objectType) ->
            lift $ pure $ Just $ CompositeObjectType objectType
        Just (UnionType unionType) ->
            lift $ pure $ Just $ CompositeUnionType unionType
        Just (InterfaceType interfaceType) ->
            lift $ pure $ Just $ CompositeInterfaceType interfaceType
        _ -> lift $ pure Nothing

fragmentDefinition
    :: Full.FragmentDefinition
    ->  State (Replacement m) (Maybe (Fragment m))
fragmentDefinition (Full.FragmentDefinition name type' _ selections) = do
    modify deleteFragmentDefinition
    fragmentSelection <- appendSelection selections
    compositeType <- lookupTypeCondition type'

    case compositeType of
        Just compositeType' -> do
            let newValue = Fragment compositeType' fragmentSelection
            modify $ insertFragment newValue
            lift $ pure $ Just newValue
        _ -> lift $ pure Nothing
  where
    deleteFragmentDefinition replacement@Replacement{..} =
        let newDefinitions = HashMap.delete name fragmentDefinitions
         in replacement{ fragmentDefinitions = newDefinitions }
    insertFragment newValue replacement@Replacement{..} =
        let newFragments = HashMap.insert name newValue fragments
         in replacement{ fragments = newFragments }

value :: forall m. Full.Value -> State (Replacement m) Type.Value
value (Full.Variable name) =
    gets (fromMaybe Type.Null . HashMap.lookup name . variableValues)
value (Full.Int int) = pure $ Type.Int int
value (Full.Float float) = pure $ Type.Float float
value (Full.String string) = pure $ Type.String string
value (Full.Boolean boolean) = pure $ Type.Boolean boolean
value Full.Null = pure Type.Null
value (Full.Enum enum) = pure $ Type.Enum enum
value (Full.List list) = Type.List <$> traverse value list
value (Full.Object object) =
    Type.Object . HashMap.fromList <$> traverse objectField object
  where
    objectField (Full.ObjectField name value') = (name,) <$> value value'

input :: forall m. Full.Value -> State (Replacement m) (Maybe Input)
input (Full.Variable name) =
    gets (fmap Variable . HashMap.lookup name . variableValues)
input (Full.Int int) = pure $ pure $ Int int
input (Full.Float float) = pure $ pure $ Float float
input (Full.String string) = pure $ pure $ String string
input (Full.Boolean boolean) = pure $ pure $ Boolean boolean
input Full.Null = pure $ pure Null
input (Full.Enum enum) = pure $ pure $ Enum enum
input (Full.List list) = pure . List <$> traverse value list
input (Full.Object object) = do
    objectFields <- foldM objectField HashMap.empty object
    pure $ pure $ Object objectFields
  where
    objectField resultMap (Full.ObjectField name value') =
        inputField resultMap name value'

inputField :: forall m
    . HashMap Full.Name Input
    -> Full.Name
    -> Full.Value
    -> State (Replacement m) (HashMap Full.Name Input)
inputField resultMap name value' = do
    objectFieldValue <-  input value'
    case objectFieldValue of
        Just fieldValue -> pure $ HashMap.insert name fieldValue resultMap
        Nothing -> pure resultMap
