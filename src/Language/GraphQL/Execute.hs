-- | This module provides functions to execute a @GraphQL@ request.
module Language.GraphQL.Execute
    ( execute
    , executeWithName
    ) where

import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Language.GraphQL.AST.Document (Document, Name)
import Language.GraphQL.Execute.Coerce
import qualified Language.GraphQL.Execute.Transform as Transform
import Language.GraphQL.Error
import Language.GraphQL.Type.Definition
import qualified Language.GraphQL.Schema as Schema
import qualified Language.GraphQL.Type.Out as Out
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
execute schema = executeRequest schema Nothing

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
executeWithName schema operationName =
    executeRequest schema (Just operationName)

executeRequest :: (Monad m, VariableValue a)
    => Schema m
    -> Maybe Text
    -> HashMap.HashMap Name a
    -> Document
    -> m Aeson.Value
executeRequest schema operationName subs document =
    case Transform.document schema operationName subs document of
        Left queryError -> pure $ singleError $ Transform.queryError queryError
        Right (Transform.Document types' rootObjectType operation)
          | (Transform.Query _ fields) <- operation ->
              executeOperation types' rootObjectType fields
          | (Transform.Mutation _ fields) <- operation ->
              executeOperation types' rootObjectType fields

-- This is actually executeMutation, but we don't distinguish between queries
-- and mutations yet.
executeOperation :: Monad m
    => HashMap Name (Type m)
    -> Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> m Aeson.Value
executeOperation types' objectType fields =
    runCollectErrs types' $ Schema.resolve Null objectType fields
