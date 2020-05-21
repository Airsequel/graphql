{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This module provides functions to execute a @GraphQL@ request.
module Language.GraphQL.Execute
    ( execute
    , executeWithName
    ) where

import qualified Data.Aeson as Aeson
import Data.Foldable (find)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Language.GraphQL.AST.Document
import qualified Language.GraphQL.AST.Core as AST.Core
import Language.GraphQL.Execute.Coerce
import qualified Language.GraphQL.Execute.Transform as Transform
import Language.GraphQL.Error
import qualified Language.GraphQL.Schema as Schema
import qualified Language.GraphQL.Type.Definition as Definition
import Language.GraphQL.Type.Schema

-- | Query error types.
data QueryError
    = OperationNotFound Text
    | OperationNameRequired
    | CoercionError

queryError :: QueryError -> Text
queryError (OperationNotFound operationName) = Text.unwords
    ["Operation", operationName, "couldn't be found in the document."]
queryError OperationNameRequired = "Missing operation name."
queryError CoercionError = "Coercion error."

-- | The substitution is applied to the document, and the resolvers are applied
-- to the resulting fields.
--
-- Returns the result of the query against the schema wrapped in a /data/
-- field, or errors wrapped in an /errors/ field.
execute :: (Monad m, VariableValue a)
    => Schema m -- ^ Resolvers.
    -> HashMap.HashMap Name a -- ^ Variable substitution function.
    -> Document -- @GraphQL@ document.
    -> m Aeson.Value
execute schema = document schema Nothing

-- | The substitution is applied to the document, and the resolvers are applied
-- to the resulting fields. The operation name can be used if the document
-- defines multiple root operations.
--
-- Returns the result of the query against the schema wrapped in a /data/
-- field, or errors wrapped in an /errors/ field.
executeWithName :: (Monad m, VariableValue a)
    => Schema m -- ^ Resolvers
    -> Text -- ^ Operation name.
    -> HashMap.HashMap Name a -- ^ Variable substitution function.
    -> Document -- ^ @GraphQL@ Document.
    -> m Aeson.Value
executeWithName schema operationName = document schema (Just operationName)

getOperation
    :: Maybe Text
    -> Transform.Document
    -> Either QueryError Transform.OperationDefinition
getOperation Nothing (Transform.Document (operation' :| []) _) = pure operation'
getOperation Nothing _ = Left OperationNameRequired
getOperation (Just operationName) (Transform.Document operations _)
    | Just operation' <- find matchingName operations = pure operation'
    | otherwise = Left $ OperationNotFound operationName
  where
    matchingName (Transform.OperationDefinition _ name _ _ _) =
        name == Just operationName

lookupInputType
    :: Type
    -> HashMap.HashMap Name (Definition.TypeDefinition m)
    -> Maybe Definition.InputType
lookupInputType (TypeNamed name) types =
    case HashMap.lookup name types of
        Just (Definition.ScalarTypeDefinition scalarType) ->
            Just $ Definition.ScalarInputType scalarType
        Just (Definition.EnumTypeDefinition enumType) ->
            Just $ Definition.EnumInputType enumType
        Just (Definition.InputObjectTypeDefinition objectType) ->
            Just $ Definition.ObjectInputType objectType
        _ -> Nothing
lookupInputType (TypeList list) types
    = Definition.ListInputType
    <$> lookupInputType list types
lookupInputType (TypeNonNull (NonNullTypeNamed nonNull)) types  =
    case HashMap.lookup nonNull types of
        Just (Definition.ScalarTypeDefinition scalarType) ->
            Just $ Definition.NonNullScalarInputType scalarType
        Just (Definition.EnumTypeDefinition enumType) ->
            Just $ Definition.NonNullEnumInputType enumType
        Just (Definition.InputObjectTypeDefinition objectType) ->
            Just $ Definition.NonNullObjectInputType objectType
        _ -> Nothing
lookupInputType (TypeNonNull (NonNullTypeList nonNull)) types
    = Definition.NonNullListInputType
    <$> lookupInputType nonNull types

coerceVariableValues :: (Monad m, VariableValue a)
    => Schema m
    -> Transform.OperationDefinition
    -> HashMap.HashMap Name a
    -> Either QueryError Schema.Subs
coerceVariableValues schema (Transform.OperationDefinition _ _ variables _ _) values =
    let referencedTypes = collectReferencedTypes schema
     in maybe (Left CoercionError) Right
        $ foldr (coerceValue referencedTypes) (Just HashMap.empty) variables
  where
    coerceValue referencedTypes variableDefinition coercedValues = do
        let VariableDefinition variableName variableTypeName _defaultValue =
                variableDefinition
        variableType <- lookupInputType variableTypeName referencedTypes
        value <- HashMap.lookup variableName values
        coercedValue <- coerceVariableValue variableType value
        HashMap.insert variableName coercedValue <$> coercedValues

executeRequest :: (Monad m, VariableValue a)
    => Schema m
    -> Maybe Text
    -> HashMap.HashMap Name a
    -> Transform.Document
    -> Either QueryError (Transform.OperationDefinition, Schema.Subs)
executeRequest schema operationName subs document' = do
    operation' <- getOperation operationName document'
    coercedValues <- coerceVariableValues schema operation' subs
    pure (operation', coercedValues)

document :: (Monad m, VariableValue a)
    => Schema m
    -> Maybe Text
    -> HashMap.HashMap Name a
    -> Document
    -> m Aeson.Value
document schema operationName subs document' =
    case Transform.document document' of
        Just transformed -> executeRequest' transformed
        Nothing -> pure $ singleError
            "The document doesn't contain any executable operations."
  where
    transformOperation fragmentTable operation' subs' =
        case Transform.operation fragmentTable subs' operation' of
            Just operationResult -> operation schema operationResult
            Nothing -> pure $ singleError "Schema transformation error."
    executeRequest' transformed@(Transform.Document _ fragmentTable) =
        case executeRequest schema operationName subs transformed of
            Right (operation', subs') -> transformOperation fragmentTable operation' subs'
            Left error' -> pure $ singleError $ queryError error'

operation :: Monad m
    => Schema m
    -> AST.Core.Operation
    -> m Aeson.Value
operation = schemaOperation
  where
    resolve queryFields = runCollectErrs
        . flip Schema.resolve queryFields
        . fmap getResolver
        . Definition.fields
    lookupError = pure
        $ singleError "Root operation type couldn't be found in the schema."
    schemaOperation Schema {query} (AST.Core.Query _ fields') =
        resolve fields' query
    schemaOperation Schema {mutation = Just mutation} (AST.Core.Mutation _ fields') =
        resolve fields' mutation
    schemaOperation Schema {mutation = Nothing} (AST.Core.Mutation _ _) =
        lookupError
    getResolver (Definition.Field _ _ _ resolver) = resolver
