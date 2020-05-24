{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | After the document is parsed, before getting executed the AST is
--   transformed into a similar, simpler AST. This module is responsible for
--   this transformation.
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
import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type.Directive as Directive
import qualified Language.GraphQL.Type.In as In
import Language.GraphQL.Type.Schema

-- | Associates a fragment name with a list of 'Core.Field's.
data Replacement = Replacement
    { fragments :: HashMap Core.Name Core.Fragment
    , fragmentDefinitions :: HashMap Full.Name Full.FragmentDefinition
    , variableValues :: Schema.Subs
    }

type TransformT a = State Replacement a

-- | GraphQL document is a non-empty list of operations.
newtype Document = Document Core.Operation

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

queryError :: QueryError -> Text
queryError (OperationNotFound operationName) = Text.unwords
    ["Operation", operationName, "couldn't be found in the document."]
queryError OperationNameRequired = "Missing operation name."
queryError CoercionError = "Coercion error."
queryError TransformationError = "Schema transformation error."
queryError EmptyDocument =
    "The document doesn't contain any executable operations."

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
    -> HashMap.HashMap Full.Name (Definition.TypeDefinition m)
    -> Maybe Definition.InputType
lookupInputType (Full.TypeNamed name) types =
    case HashMap.lookup name types of
        Just (Definition.ScalarTypeDefinition scalarType) ->
            Just $ Definition.ScalarInputType scalarType
        Just (Definition.EnumTypeDefinition enumType) ->
            Just $ Definition.EnumInputType enumType
        Just (Definition.InputObjectTypeDefinition objectType) ->
            Just $ Definition.ObjectInputType objectType
        _ -> Nothing
lookupInputType (Full.TypeList list) types
    = Definition.ListInputType
    <$> lookupInputType list types
lookupInputType (Full.TypeNonNull (Full.NonNullTypeNamed nonNull)) types  =
    case HashMap.lookup nonNull types of
        Just (Definition.ScalarTypeDefinition scalarType) ->
            Just $ Definition.NonNullScalarInputType scalarType
        Just (Definition.EnumTypeDefinition enumType) ->
            Just $ Definition.NonNullEnumInputType enumType
        Just (Definition.InputObjectTypeDefinition objectType) ->
            Just $ Definition.NonNullObjectInputType objectType
        _ -> Nothing
lookupInputType (Full.TypeNonNull (Full.NonNullTypeList nonNull)) types
    = Definition.NonNullListInputType
    <$> lookupInputType nonNull types

coerceVariableValues :: (Monad m, VariableValue a)
    => Schema m
    -> OperationDefinition
    -> HashMap.HashMap Full.Name a
    -> Either QueryError Schema.Subs
coerceVariableValues schema operationDefinition variableValues' =
    let referencedTypes = collectReferencedTypes schema
        OperationDefinition _ _ variableDefinitions _ _ = operationDefinition
        coerceValue' = coerceValue referencedTypes
     in maybe (Left CoercionError) Right
        $ foldr coerceValue' (Just HashMap.empty) variableDefinitions
  where
    coerceValue referencedTypes variableDefinition coercedValues = do
        let Full.VariableDefinition variableName variableTypeName defaultValue =
                variableDefinition
        let defaultValue' = constValue <$> defaultValue
        let value' = HashMap.lookup variableName variableValues'

        variableType <- lookupInputType variableTypeName referencedTypes
        HashMap.insert variableName
            <$> choose value' defaultValue' variableType
            <*> coercedValues
    choose Nothing defaultValue variableType
        | Just _ <- defaultValue = defaultValue
        | not (isNonNullInputType variableType) = Just In.Null
    choose (Just value') _ variableType
        | Just coercedValue <- coerceVariableValue variableType value'
        , not (isNonNullInputType variableType) || coercedValue /= In.Null =
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
document :: (Monad m, VariableValue a)
    => Schema m
    -> Maybe Full.Name
    -> HashMap Full.Name a
    -> Full.Document
    -> Either QueryError Document
document schema operationName subs ast = do
    let (operations, fragmentTable) = foldr defragment ([], HashMap.empty) ast
    nonEmptyOperations <- maybe (Left EmptyDocument) Right
        $ NonEmpty.nonEmpty operations
    chosenOperation <- getOperation operationName nonEmptyOperations
    coercedValues <- coerceVariableValues schema chosenOperation subs

    pure $ Document
        $ operation fragmentTable coercedValues chosenOperation
  where
    defragment definition (operations, fragments')
        | (Full.ExecutableDefinition executable) <- definition
        , (Full.DefinitionOperation operation') <- executable =
            (transform operation' : operations, fragments')
        | (Full.ExecutableDefinition executable) <- definition
        , (Full.DefinitionFragment fragment) <- executable
        , (Full.FragmentDefinition name _ _ _) <- fragment =
            (operations, HashMap.insert name fragment fragments')
    defragment _ acc = acc
    transform = \case
        Full.OperationDefinition type' name variables directives' selections ->
            OperationDefinition type' name variables directives' selections
        Full.SelectionSet selectionSet ->
            OperationDefinition Full.Query Nothing mempty mempty selectionSet

-- * Operation

operation
    :: HashMap Full.Name Full.FragmentDefinition
    -> Schema.Subs
    -> OperationDefinition
    -> Core.Operation
operation fragmentTable subs operationDefinition
    = runIdentity
    $ evalStateT (collectFragments >> transform operationDefinition)
    $ Replacement HashMap.empty fragmentTable subs
  where
    transform :: OperationDefinition -> TransformT Core.Operation
    transform (OperationDefinition Full.Query name _ _ sels) =
        Core.Query name <$> appendSelection sels
    transform (OperationDefinition Full.Mutation name _ _ sels) =
        Core.Mutation name <$> appendSelection sels

-- * Selection

selection ::
    Full.Selection ->
    TransformT (Either (Seq Core.Selection) Core.Selection)
selection (Full.Field alias name arguments' directives' selections) =
    maybe (Left mempty) (Right . Core.SelectionField) <$> do
        fieldArguments <- arguments arguments'
        fieldSelections <- appendSelection selections
        fieldDirectives <- Directive.selection <$> directives directives'
        let field' = Core.Field alias name fieldArguments fieldSelections
        pure $ field' <$ fieldDirectives
selection (Full.FragmentSpread name directives') =
    maybe (Left mempty) (Right . Core.SelectionFragment) <$> do
        spreadDirectives <- Directive.selection <$> directives directives'
        fragments' <- gets fragments

        fragmentDefinitions' <- gets fragmentDefinitions
        case HashMap.lookup name fragments' of
            Just definition -> lift $ pure $ definition <$ spreadDirectives
            Nothing -> case HashMap.lookup name fragmentDefinitions' of
                         Just definition -> do
                             fragment <- fragmentDefinition definition
                             lift $ pure $ fragment <$ spreadDirectives
                         Nothing -> lift $ pure  Nothing
selection (Full.InlineFragment type' directives' selections) = do
    fragmentDirectives <- Directive.selection <$> directives directives'
    case fragmentDirectives of
        Nothing -> pure $ Left mempty
        _ -> do
            fragmentSelectionSet <- appendSelection selections
            pure $ maybe Left selectionFragment type' fragmentSelectionSet
  where
    selectionFragment typeName = Right
        . Core.SelectionFragment
        . Core.Fragment typeName

appendSelection ::
    Traversable t =>
    t Full.Selection ->
    TransformT (Seq Core.Selection)
appendSelection = foldM go mempty
  where
    go acc sel = append acc <$> selection sel
    append acc (Left list) = list >< acc
    append acc (Right one) = one <| acc

directives :: [Full.Directive] -> TransformT [Core.Directive]
directives = traverse directive
  where
    directive (Full.Directive directiveName directiveArguments) =
        Core.Directive directiveName <$> arguments directiveArguments

-- * Fragment replacement

-- | Extract fragment definitions into a single 'HashMap'.
collectFragments :: TransformT ()
collectFragments = do
    fragDefs <- gets fragmentDefinitions
    let nextValue = head $ HashMap.elems fragDefs
    unless (HashMap.null fragDefs) $ do
        _ <- fragmentDefinition nextValue
        collectFragments

fragmentDefinition ::
    Full.FragmentDefinition ->
    TransformT Core.Fragment
fragmentDefinition (Full.FragmentDefinition name type' _ selections) = do
    modify deleteFragmentDefinition
    fragmentSelection <- appendSelection selections
    let newValue = Core.Fragment type' fragmentSelection
    modify $ insertFragment newValue
    lift $ pure newValue
  where
    deleteFragmentDefinition (Replacement fragments' fragmentDefinitions' subs) =
        Replacement fragments' (HashMap.delete name fragmentDefinitions') subs
    insertFragment newValue (Replacement fragments' fragmentDefinitions' subs) =
        let newFragments = HashMap.insert name newValue fragments'
         in Replacement newFragments fragmentDefinitions' subs

arguments :: [Full.Argument] -> TransformT Core.Arguments
arguments = fmap Core.Arguments . foldM go HashMap.empty
  where
    go arguments' (Full.Argument name value') = do
        substitutedValue <- value value'
        return $ HashMap.insert name substitutedValue arguments'

value :: Full.Value -> TransformT In.Value
value (Full.Variable name) =
    gets $ fromMaybe In.Null . HashMap.lookup name . variableValues
value (Full.Int i) = pure $ In.Int i
value (Full.Float f) = pure $ In.Float f
value (Full.String x) = pure $ In.String x
value (Full.Boolean b) = pure $ In.Boolean b
value Full.Null = pure   In.Null
value (Full.Enum e) = pure $ In.Enum e
value (Full.List l) =
    In.List <$> traverse value l
value (Full.Object o) =
    In.Object . HashMap.fromList <$> traverse objectField o

objectField :: Full.ObjectField Full.Value -> TransformT (Core.Name, In.Value)
objectField (Full.ObjectField name value') = (name,) <$> value value'
