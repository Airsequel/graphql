{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This module provides functions to execute a @GraphQL@ request.
module Language.GraphQL.Execute
    ( execute
    , executeWithName
    ) where

import qualified Data.Aeson as Aeson
import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Language.GraphQL.AST.Document
import qualified Language.GraphQL.AST.Core as AST.Core
import qualified Language.GraphQL.Execute.Transform as Transform
import Language.GraphQL.Error
import qualified Language.GraphQL.Schema as Schema
import Language.GraphQL.Type.Definition
import Language.GraphQL.Type.Schema

-- | Query error types.
data QueryError
    = OperationNotFound Text
    | OperationNameRequired

queryError :: QueryError -> Text
queryError (OperationNotFound operationName) = Text.unwords
    ["Operation", operationName, "couldn't be found in the document."]
queryError OperationNameRequired = "Missing operation name."

-- | The substitution is applied to the document, and the resolvers are applied
-- to the resulting fields.
--
-- Returns the result of the query against the schema wrapped in a /data/
-- field, or errors wrapped in an /errors/ field.
execute :: Monad m
    => Schema m -- ^ Resolvers.
    -> Schema.Subs -- ^ Variable substitution function.
    -> Document -- @GraphQL@ document.
    -> m Aeson.Value
execute schema subs doc =
    maybe transformError (document schema Nothing)
        $ Transform.document subs doc
  where
    transformError = return $ singleError "Schema transformation error."

-- | The substitution is applied to the document, and the resolvers are applied
-- to the resulting fields. The operation name can be used if the document
-- defines multiple root operations.
--
-- Returns the result of the query against the schema wrapped in a /data/
-- field, or errors wrapped in an /errors/ field.
executeWithName :: Monad m
    => Schema m -- ^ Resolvers
    -> Text -- ^ Operation name.
    -> Schema.Subs -- ^ Variable substitution function.
    -> Document -- ^ @GraphQL@ Document.
    -> m Aeson.Value
executeWithName schema operationName subs doc =
    maybe transformError (document schema $ Just operationName)
        $ Transform.document subs doc
  where
    transformError = return $ singleError "Schema transformation error."

getOperation
    :: Maybe Text
    -> AST.Core.Document
    -> Either QueryError AST.Core.Operation
getOperation Nothing (operation' :| []) = pure operation'
getOperation Nothing _ = Left OperationNameRequired
getOperation (Just operationName) document'
    | Just operation' <- find matchingName document' = pure operation'
    | otherwise = Left $ OperationNotFound operationName
  where
    matchingName (AST.Core.Query (Just name') _) = operationName == name'
    matchingName (AST.Core.Mutation (Just name') _) = operationName == name'
    matchingName _ = False

document :: Monad m
    => Schema m
    -> Maybe Text
    -> AST.Core.Document
    -> m Aeson.Value
document schema operationName document' =
    case getOperation operationName document' of
        Left error' -> pure $ singleError $ queryError error'
        Right operation' -> operation schema operation'

operation :: Monad m
    => Schema m
    -> AST.Core.Operation
    -> m Aeson.Value
operation = schemaOperation
  where
    resolve queryFields = runCollectErrs
        . flip Schema.resolve queryFields
        . fields
    lookupError = pure
        $ singleError "Root operation type couldn't be found in the schema."
    schemaOperation Schema {query} (AST.Core.Query _ fields') =
        resolve fields' query
    schemaOperation Schema {mutation = Just mutation} (AST.Core.Mutation _ fields') =
        resolve fields' mutation
    schemaOperation Schema {mutation = Nothing} (AST.Core.Mutation _ _) =
        lookupError
