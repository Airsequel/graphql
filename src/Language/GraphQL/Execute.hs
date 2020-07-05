-- | This module provides functions to execute a @GraphQL@ request.
module Language.GraphQL.Execute
    ( execute
    , module Language.GraphQL.Execute.Coerce
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Language.GraphQL.AST.Document (Document, Name)
import Language.GraphQL.Execute.Coerce
import Language.GraphQL.Execute.Execution
import qualified Language.GraphQL.Execute.Transform as Transform
import Language.GraphQL.Error
import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema

-- | The substitution is applied to the document, and the resolvers are applied
-- to the resulting fields. The operation name can be used if the document
-- defines multiple root operations.
--
-- Returns the result of the query against the schema wrapped in a /data/
-- field, or errors wrapped in an /errors/ field.
execute :: (Monad m, VariableValue a, Serialize b)
    => Schema m -- ^ Resolvers.
    -> Maybe Text -- ^ Operation name.
    -> HashMap.HashMap Name a -- ^ Variable substitution function.
    -> Document -- @GraphQL@ document.
    -> m (Response b)
execute schema operationName subs document =
    case Transform.document schema operationName subs document of
        Left queryError -> pure $ singleError $ Transform.queryError queryError
        Right transformed -> executeRequest transformed

executeRequest :: (Monad m, Serialize a)
    => Transform.Document m
    -> m (Response a)
executeRequest (Transform.Document types' rootObjectType operation)
    | (Transform.Query _ fields) <- operation =
        executeOperation types' rootObjectType fields
    | (Transform.Mutation _ fields) <- operation =
        executeOperation types' rootObjectType fields

-- This is actually executeMutation, but we don't distinguish between queries
-- and mutations yet.
executeOperation :: (Monad m, Serialize a)
    => HashMap Name (Type m)
    -> Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> m (Response a)
executeOperation types' objectType fields =
    runCollectErrs types' $ executeSelectionSet Definition.Null objectType fields
