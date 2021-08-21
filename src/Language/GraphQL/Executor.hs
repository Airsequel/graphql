{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.GraphQL.Executor
   ( Error(..)
   , Operation(..)
   , QueryError(..)
   , Response(..)
   , Segment(..)
   , executeRequest
   ) where

import qualified Language.GraphQL.AST.Document as Full
import qualified Data.Aeson as Aeson
import Data.Foldable (find)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int32)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.GraphQL.Execute.Coerce as Coerce
import Language.GraphQL.Execute.OrderedMap (OrderedMap)
import qualified Language.GraphQL.Type.Out as Out
import qualified Language.GraphQL.Type as Type
import qualified Language.GraphQL.Type.Internal as Type.Internal
import Language.GraphQL.Type.Schema (Schema)
import qualified Language.GraphQL.Type.Schema as Schema

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
    | FragmentSpreadSelection FragmentSpread
    | InlineFragmentSelection InlineFragment

data Argument = Argument Full.Name (Full.Node Value) Full.Location

data Field =
    Field (Maybe Full.Name) Full.Name [Argument] [Directive] SelectionSet Full.Location

data InlineFragment = InlineFragment
    (Maybe Full.TypeCondition) [Directive] SelectionSet Full.Location

data FragmentSpread = FragmentSpread Full.Name [Directive] Full.Location

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

data Directive = Directive Full.Name [Argument] Full.Location

document :: Full.Document -> [Full.OperationDefinition]
document = foldr filterOperation []
  where
    filterOperation (Full.ExecutableDefinition executableDefinition) accumulator
        | Full.DefinitionOperation operationDefinition' <- executableDefinition =
            operationDefinition' : accumulator
    filterOperation _ accumulator = accumulator -- Fragment.

operationDefinition :: Type.Subs -> Full.OperationDefinition -> Operation
operationDefinition coercedVariableValues = \case
    Full.OperationDefinition operationType _ _ _ selectionSet' _ ->
        Operation operationType coercedVariableValues
        $ selectionSet selectionSet'
    Full.SelectionSet selectionSet' _ ->
        Operation Full.Query coercedVariableValues (selectionSet selectionSet')

selectionSet :: Full.SelectionSet -> SelectionSet
selectionSet = NonEmpty.toList . fmap selection

selection :: Full.Selection -> Selection
selection (Full.FieldSelection field') = FieldSelection $ field field'
selection (Full.FragmentSpreadSelection fragmentSpread') =
    FragmentSpreadSelection $ fragmentSpread fragmentSpread'
selection (Full.InlineFragmentSelection inlineFragment') =
    InlineFragmentSelection $ inlineFragment inlineFragment'

inlineFragment :: Full.InlineFragment -> InlineFragment
inlineFragment (Full.InlineFragment typeCondition directives selectionSet' location) =
    InlineFragment
        typeCondition
        (directive <$> directives)
        (selectionSet selectionSet')
        location

fragmentSpread :: Full.FragmentSpread -> FragmentSpread
fragmentSpread (Full.FragmentSpread name' directives location) =
    FragmentSpread name' (directive <$> directives) location

field :: Full.Field -> Field
field (Full.Field alias' name' arguments' directives' selectionSet' location') =
    Field
        alias'
        name'
        (argument <$> arguments')
        (directive <$> directives')
        (selection <$> selectionSet')
        location'

argument :: Full.Argument -> Argument
argument (Full.Argument name' valueNode location') =
    Argument name' (node valueNode) location'

directive :: Full.Directive -> Directive
directive (Full.Directive name' arguments location') =
    Directive name' (argument <$> arguments) location'

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

node :: Full.Node Full.Value -> Full.Node Value
node Full.Node{node = node', ..} = Full.Node (variableValue node') location

objectField :: Full.ObjectField Full.Value -> ObjectField
objectField Full.ObjectField{..} = ObjectField
    { name = name
    , value = node value
    , location = location
    }

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
    transformedDocument = document sourceDocument
    operationAndVariables = do
        operationDefinition' <- getOperation transformedDocument operationName
        coercedVariableValues <- coerceVariableValues
            schemaTypes
            operationDefinition'
            variableValues
        pure $ operationDefinition coercedVariableValues operationDefinition'

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

collectFields :: Out.ObjectType IO -> SelectionSet -> Type.Subs -> OrderedMap [Selection]
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
