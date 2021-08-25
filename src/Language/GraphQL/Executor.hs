{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.GraphQL.Executor
   ( Error(..)
   , Operation(..)
   , QueryError(..)
   , Response(..)
   , Segment(..)
   , executeRequest
   ) where

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..), local, runReader)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad (foldM)
import qualified Language.GraphQL.AST.Document as Full
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.Foldable (find)
import Data.Functor.Identity (Identity)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.GraphQL.Execute.Coerce as Coerce
import Language.GraphQL.Execute.OrderedMap (OrderedMap)
import qualified Language.GraphQL.Type.Out as Out
import qualified Language.GraphQL.Type as Type
import qualified Language.GraphQL.Type.Internal as Type.Internal
import Language.GraphQL.Type.Schema (Schema)
import qualified Language.GraphQL.Type.Schema as Schema

data Replacement = Replacement
    { variableValues :: Type.Subs
    , fragmentDefinitions :: HashMap Full.Name Full.FragmentDefinition
    , visitedFragments :: HashSet Full.Name
    }

newtype TransformT m a = TransformT
    { runTransformT :: ReaderT Replacement m a
    }

instance Functor m => Functor (TransformT m) where
    fmap f = TransformT . fmap f . runTransformT

instance Applicative m => Applicative (TransformT m) where
    pure = TransformT . pure
    TransformT f <*> TransformT x = TransformT $ f <*> x

instance Monad m => Monad (TransformT m) where
    TransformT x >>= f = TransformT $ x >>= runTransformT . f

instance MonadTrans TransformT where
    lift = TransformT . lift

type Transform = TransformT Identity

data Segment = Segment String | Index Int

data Error = Error
   { message :: String
   , locations :: [Full.Location]
   , path :: [Segment]
   }

data Response = Response
   { data' :: Aeson.Object
   , errors :: [Error]
   }

data QueryError
   = OperationNameRequired
   | OperationNotFound String
   | CoercionError Full.VariableDefinition
   | UnknownInputType Full.VariableDefinition

asks :: forall a. (Replacement -> a) -> Transform a
asks = TransformT . Reader.asks

queryError :: QueryError -> Error
queryError OperationNameRequired =
    Error{ message = "Operation name is required.", locations = [], path = [] }
queryError (OperationNotFound operationName) =
    let queryErrorMessage = concat
            [ "Operation \""
            ,  operationName
            , "\" not found."
            ]
     in Error{ message = queryErrorMessage, locations = [], path = [] }
queryError (CoercionError variableDefinition) =
    let Full.VariableDefinition variableName _ _ location = variableDefinition
        queryErrorMessage = concat
            [ "Failed to coerce the variable \""
            , Text.unpack variableName
            , "\"."
            ]
     in Error{ message = queryErrorMessage, locations = [location], path = [] }
queryError (UnknownInputType variableDefinition) =
    let Full.VariableDefinition variableName variableTypeName _ location = variableDefinition
        queryErrorMessage = concat
            [ "Variable \""
            , Text.unpack variableName
            , "\" has unknown type \""
            , show variableTypeName
            , "\"."
            ]
     in Error{ message = queryErrorMessage, locations = [location], path = [] }

respondWithQueryError :: QueryError -> Response
respondWithQueryError = Response mempty . pure . queryError

-- operationName selectionSet location
data Operation = Operation
    Full.OperationType
    Type.Subs
    SelectionSet

type SelectionSet = [Selection]

data Selection
    = FieldSelection Field
    | FragmentSelection Fragment

data Argument = Argument Full.Name (Full.Node Value) Full.Location

data Field = Field
    (Maybe Full.Name)
    Full.Name
    [Argument]
    SelectionSet
    Full.Location

data Fragment = Fragment
    (Maybe Full.TypeCondition) SelectionSet Full.Location

data Value
    = Variable Full.Name
    | Int Int32
    | Float Double
    | String Text
    | Boolean Bool
    | Null
    | Enum Full.Name
    | List [Full.Node Value]
    | Object [ObjectField]

data ObjectField = ObjectField
    { name :: Full.Name
    , value :: Full.Node Value
    , location :: Full.Location
    }

document :: Full.Document
    -> ([Full.OperationDefinition], HashMap Full.Name Full.FragmentDefinition)
document = foldr filterOperation ([], HashMap.empty)
  where
    filterOperation (Full.ExecutableDefinition executableDefinition) accumulator
        | Full.DefinitionOperation operationDefinition' <- executableDefinition =
            first (operationDefinition' :) accumulator
        | Full.DefinitionFragment fragmentDefinition <- executableDefinition
        , Full.FragmentDefinition fragmentName _ _ _ _ <- fragmentDefinition =
            HashMap.insert fragmentName fragmentDefinition <$> accumulator
    filterOperation _ accumulator = accumulator -- Type system definitions.

transform :: Full.OperationDefinition -> Transform Operation
transform (Full.OperationDefinition operationType _ _ _ selectionSet' _) = do
    coercedVariableValues <- asks variableValues
    transformedSelections <- selectionSet selectionSet'
    pure $ Operation operationType coercedVariableValues transformedSelections
transform (Full.SelectionSet selectionSet' _) = do
    coercedVariableValues <- asks variableValues
    transformedSelections <- selectionSet selectionSet'
    pure $ Operation Full.Query coercedVariableValues transformedSelections

selectionSet :: Full.SelectionSet -> Transform SelectionSet
selectionSet = fmap catMaybes . traverse selection . NonEmpty.toList

selection :: Full.Selection -> Transform (Maybe Selection)
selection (Full.FieldSelection field') = fmap FieldSelection <$> field field'
selection (Full.FragmentSpreadSelection fragmentSpread') =
    fmap FragmentSelection <$> fragmentSpread fragmentSpread'
selection (Full.InlineFragmentSelection inlineFragment') =
   fmap FragmentSelection <$> inlineFragment inlineFragment'

directives :: [Full.Directive] -> Transform (Maybe [Type.Directive])
directives = fmap Type.selection . traverse directive

inlineFragment :: Full.InlineFragment -> Transform (Maybe Fragment)
inlineFragment (Full.InlineFragment typeCondition directives' selectionSet' location) = do
    transformedSelections <- selectionSet selectionSet'
    transformedDirectives <- directives directives'
    pure $ transformedDirectives
        >> pure (Fragment typeCondition transformedSelections location)

fragmentSpread :: Full.FragmentSpread -> Transform (Maybe Fragment)
fragmentSpread (Full.FragmentSpread spreadName directives' location) = do
    transformedDirectives <- directives directives'
    visitedFragment <- asks $ HashSet.member spreadName . visitedFragments
    possibleFragmentDefinition <- asks
        $ HashMap.lookup spreadName
        . fragmentDefinitions
    case transformedDirectives >> possibleFragmentDefinition of
        Just (Full.FragmentDefinition _ typeCondition _ selections _)
            | visitedFragment -> pure Nothing
            | otherwise -> do
                transformedSelections <- TransformT
                    $ local fragmentInserter
                    $ runTransformT
                    $ selectionSet selections
                pure $ Just $ Fragment
                    (Just typeCondition)
                    transformedSelections
                    location
        Nothing ->
            pure Nothing
  where
    fragmentInserter replacement@Replacement{ visitedFragments } = replacement
        { visitedFragments = HashSet.insert spreadName visitedFragments }


field :: Full.Field -> Transform (Maybe Field)
field (Full.Field alias' name' arguments' directives' selectionSet' location') = do
    transformedSelections <- catMaybes <$> traverse selection selectionSet'
    transformedDirectives <- directives directives'
    let transformedField = Field
            alias'
            name'
            transformedArguments
            transformedSelections
            location'
    pure $ transformedDirectives >> pure transformedField
  where
    transformedArguments = argument <$> arguments'

argument :: Full.Argument -> Argument
argument (Full.Argument name' valueNode location') =
    Argument name' (node valueNode) location'

directive :: Full.Directive -> Transform Type.Directive
directive (Full.Directive name' arguments _)
    = Type.Directive name'
    . Type.Arguments
    <$> foldM go HashMap.empty arguments
  where
    go accumulator (Full.Argument argumentName Full.Node{ node = node' } _) = do
        transformedValue <- directiveValue node'
        pure $ HashMap.insert argumentName transformedValue accumulator

directiveValue :: Full.Value -> Transform Type.Value
directiveValue = \case
    (Full.Variable name') -> asks
        $ HashMap.lookupDefault Type.Null name'
        . variableValues
    (Full.Int integer) -> pure $ Type.Int integer
    (Full.Float double) -> pure $ Type.Float double
    (Full.String string) -> pure $ Type.String string
    (Full.Boolean boolean) -> pure $ Type.Boolean boolean
    Full.Null -> pure Type.Null
    (Full.Enum enum) -> pure $ Type.Enum enum
    (Full.List list) -> Type.List <$> traverse directiveNode list
    (Full.Object objectFields) ->
        Type.Object <$> foldM objectField HashMap.empty objectFields
  where
    directiveNode Full.Node{ node = node'} = directiveValue node'
    objectField accumulator Full.ObjectField{ name, value } = do
        transformedValue <- directiveNode value
        pure $ HashMap.insert name transformedValue accumulator

variableValue :: Full.Value -> Value
variableValue (Full.Variable name') = Variable name'
variableValue (Full.Int integer) = Int integer
variableValue (Full.Float double) = Float double
variableValue (Full.String string) = String string
variableValue (Full.Boolean boolean) = Boolean boolean
variableValue Full.Null = Null
variableValue (Full.Enum enum) = Enum enum
variableValue (Full.List list) = List $ node <$> list
variableValue (Full.Object objectFields) = Object $ objectField <$> objectFields
  where
    objectField :: Full.ObjectField Full.Value -> ObjectField
    objectField Full.ObjectField{..} = ObjectField
        { name = name
        , value = node value
        , location = location
        }

node :: Full.Node Full.Value -> Full.Node Value
node Full.Node{node = node', ..} = Full.Node (variableValue node') location

executeRequest :: Schema IO
    -> Full.Document
    -> Maybe String
    -> Aeson.Object
    -> Aeson.Object
    -> IO Response
executeRequest schema sourceDocument operationName variableValues initialValue =
    case operationAndVariables of
        Left queryError' -> pure $ respondWithQueryError queryError'
        Right operation
            | Operation Full.Query coercedVariables topSelections <- operation ->
                executeQuery topSelections schema coercedVariables initialValue
            | Operation Full.Mutation corecedVariables topSelections <- operation ->
                executeMutation topSelections schema corecedVariables initialValue
            | Operation Full.Subscription coercedVariables topSelections <- operation ->
                subscribe topSelections schema coercedVariables initialValue
  where
    schemaTypes = Schema.types schema
    (operationDefinitions, fragmentDefinitions') = document sourceDocument
    operationAndVariables = do
        operationDefinition <- getOperation operationDefinitions operationName
        coercedVariableValues <- coerceVariableValues
            schemaTypes
            operationDefinition
            variableValues
        let replacement = Replacement
                { variableValues = coercedVariableValues
                , fragmentDefinitions = fragmentDefinitions'
                , visitedFragments = mempty
                }
        pure
            $ flip runReader replacement
            $ runTransformT
            $ transform operationDefinition

getOperation :: [Full.OperationDefinition] -> Maybe String -> Either QueryError Full.OperationDefinition
getOperation [operation] Nothing = Right operation
getOperation operations (Just givenOperationName)
    = maybe (Left $ OperationNotFound givenOperationName) Right
    $ find findOperationByName operations
  where
    findOperationByName (Full.OperationDefinition _ (Just operationName) _ _ _ _) =
        givenOperationName == Text.unpack operationName
    findOperationByName _ = False
getOperation _ _ = Left OperationNameRequired

executeQuery :: SelectionSet
    -> Schema IO
    -> Type.Subs
    -> Aeson.Object
    -> IO Response
executeQuery topSelections schema coercedVariables initialValue =
    let queryType = Schema.query schema
        _data = executeSelectionSet topSelections queryType initialValue coercedVariables
     in pure $ Response mempty mempty

executeMutation :: forall m
    . SelectionSet
    -> Schema m
    -> Type.Subs
    -> Aeson.Object
    -> IO Response
executeMutation _operation _schema _coercedVariableValues _initialValue =
    pure $ Response mempty mempty

subscribe :: forall m
    . SelectionSet
    -> Schema m
    -> Type.Subs
    -> Aeson.Object
    -> IO Response
subscribe _operation _schema _coercedVariableValues _initialValue =
    pure $ Response mempty mempty

executeSelectionSet
    :: SelectionSet
    -> Out.ObjectType IO
    -> Aeson.Object
    -> Type.Subs
    -> Aeson.Object
executeSelectionSet selections objectType _objectValue variableValues =
    let _groupedFieldSet = collectFields objectType selections variableValues
     in mempty

collectFields :: Out.ObjectType IO -> SelectionSet -> Type.Subs -> OrderedMap (NonEmpty Selection)
collectFields = mempty

coerceVariableValues :: Coerce.VariableValue a
    => forall m
    . HashMap Full.Name (Schema.Type m)
    -> Full.OperationDefinition
    -> HashMap Full.Name a
    -> Either QueryError Type.Subs
coerceVariableValues types operationDefinition' variableValues
    | Full.OperationDefinition _ _ variableDefinitions _ _ _ <-
        operationDefinition'
    = foldr forEach (Right HashMap.empty) variableDefinitions
    | otherwise = pure mempty
  where
    forEach variableDefinition (Right coercedValues) =
        let Full.VariableDefinition variableName variableTypeName defaultValue _ =
                variableDefinition
            defaultValue' = constValue . Full.node <$> defaultValue
         in case Type.Internal.lookupInputType variableTypeName types of
            Just variableType ->
                maybe (Left $ CoercionError variableDefinition) Right
                    $ Coerce.matchFieldValues
                        coerceVariableValue'
                        variableValues
                        variableName
                        variableType
                        defaultValue'
                    $ Just coercedValues
            Nothing -> Left $ UnknownInputType variableDefinition
    forEach _ coercedValuesOrError = coercedValuesOrError
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
constValue (Full.ConstList list) = Type.List $ constValue . Full.node <$> list
constValue (Full.ConstObject o) =
    Type.Object $ HashMap.fromList $ constObjectField <$> o
  where
    constObjectField Full.ObjectField{value = value', ..} =
        (name, constValue $ Full.node value')
