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
   , coerceVariableValues
   , executeRequest
   ) where

import qualified Language.GraphQL.AST.Document as Full
import qualified Data.Aeson as Aeson
import Data.Foldable (find)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Language.GraphQL.Execute.Coerce as Coerce
import qualified Language.GraphQL.Type as Type
import qualified Language.GraphQL.Type.Internal as Type.Internal
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
   | CoercionError

instance Show QueryError where
    show OperationNameRequired = "Operation name is required."
    show (OperationNotFound operationName) =
        concat ["Operation \"",  operationName, "\" not found."]
    show CoercionError = "Coercion error."

respondWithQueryError :: QueryError -> Response
respondWithQueryError queryError
    = Response mempty
    $ pure
    $ Error{ message = show queryError, locations = [], path = [] }

-- operationName selectionSet location
data Operation = Operation
    Full.OperationType
    (Maybe String)
    [Full.VariableDefinition]
    Full.SelectionSet
    Full.Location

document :: Full.Document -> [Operation]
document = foldr filterOperation []
  where
    filterOperation (Full.ExecutableDefinition executableDefinition) accumulator
        | Full.DefinitionOperation operationDefinition' <- executableDefinition =
           operationDefinition operationDefinition' : accumulator
    filterOperation _ accumulator = accumulator -- Fragment.

operationDefinition :: Full.OperationDefinition -> Operation
operationDefinition = \case
    Full.OperationDefinition operationType operationName variables _ selectionSet operationLocation ->
        let maybeOperationName = Text.unpack <$> operationName
         in Operation operationType maybeOperationName variables selectionSet operationLocation
    Full.SelectionSet selectionSet operationLocation ->
        Operation Full.Query Nothing [] selectionSet operationLocation

executeRequest :: Type.Internal.Schema IO
    -> Full.Document
    -> Maybe String
    -> Aeson.Object
    -> Aeson.Object
    -> IO Response
executeRequest _schema sourceDocument operationName _variableValues _initialValue =
   let transformedDocument = document sourceDocument
       operation = getOperation transformedDocument operationName
    in case operation of
        Left queryError -> pure $ respondWithQueryError queryError
        Right (Operation Full.Query _ _ _ _) -> executeQuery
        Right (Operation Full.Mutation _ _ _ _) -> executeMutation
        Right (Operation Full.Subscription _ _ _ _) -> subscribe

getOperation :: [Operation] -> Maybe String -> Either QueryError Operation
getOperation [operation] Nothing = Right operation
getOperation operations (Just givenOperationName)
    = maybe (Left $ OperationNotFound givenOperationName) Right
    $ find findOperationByName operations
  where
    findOperationByName (Operation _ (Just operationName) _ _ _) =
        givenOperationName == operationName
    findOperationByName _ = False
getOperation _ _ = Left OperationNameRequired

executeQuery :: IO Response
executeQuery = pure $ Response mempty mempty

executeMutation :: IO Response
executeMutation = pure $ Response mempty mempty

subscribe :: IO Response
subscribe = pure $ Response mempty mempty

coerceVariableValues :: Coerce.VariableValue a
    => forall m
    . HashMap Full.Name (Schema.Type m)
    -> Operation
    -> HashMap Full.Name a
    -> Either QueryError Type.Subs
coerceVariableValues types operationDefinition' variableValues =
    let Operation _ _ variableDefinitions _ _ = operationDefinition'
     in maybe (Left CoercionError) Right
        $ foldr forEach (Just HashMap.empty) variableDefinitions
  where
    forEach variableDefinition coercedValues = do
        let Full.VariableDefinition variableName variableTypeName defaultValue _ =
                variableDefinition
        let defaultValue' = constValue . Full.node <$> defaultValue
        variableType <- Type.Internal.lookupInputType variableTypeName types

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
constValue (Full.ConstList list) = Type.List $ constValue . Full.node <$> list
constValue (Full.ConstObject o) =
    Type.Object $ HashMap.fromList $ constObjectField <$> o
  where
    constObjectField Full.ObjectField{value = value', ..} =
        (name, constValue $ Full.node value')
