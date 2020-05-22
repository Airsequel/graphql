{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This module provides functions to execute a @GraphQL@ request.
module Language.GraphQL.Execute
    ( execute
    , executeWithName
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Language.GraphQL.AST.Document
import qualified Language.GraphQL.AST.Core as AST.Core
import Language.GraphQL.Execute.Coerce
import qualified Language.GraphQL.Execute.Transform as Transform
import Language.GraphQL.Error
import qualified Language.GraphQL.Schema as Schema
import qualified Language.GraphQL.Type.Definition as Definition
import Language.GraphQL.Type.Schema

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

document :: (Monad m, VariableValue a)
    => Schema m
    -> Maybe Text
    -> HashMap.HashMap Name a
    -> Document
    -> m Aeson.Value
document schema operationName subs document' =
    case Transform.document schema operationName subs document' of
        Left queryError -> pure $ singleError $ Transform.queryError queryError
        Right (Transform.Document op _) -> operation schema op

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
