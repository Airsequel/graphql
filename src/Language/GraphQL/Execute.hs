-- | This module provides functions to execute a @GraphQL@ request.
module Language.GraphQL.Execute
    ( execute
    , module Language.GraphQL.Execute.Coerce
    ) where

import Control.Monad.Catch (MonadCatch)
import Data.HashMap.Strict (HashMap)
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Language.GraphQL.AST.Document (Document, Name)
import Language.GraphQL.Execute.Coerce
import Language.GraphQL.Execute.Execution
import Language.GraphQL.Execute.Internal
import qualified Language.GraphQL.Execute.Transform as Transform
import qualified Language.GraphQL.Execute.Subscribe as Subscribe
import Language.GraphQL.Error (ResponseEventStream, Response, runCollectErrs)
import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema

-- | The substitution is applied to the document, and the resolvers are applied
-- to the resulting fields. The operation name can be used if the document
-- defines multiple root operations.
--
-- Returns the result of the query against the schema wrapped in a /data/
-- field, or errors wrapped in an /errors/ field.
execute :: (MonadCatch m, VariableValue a, Serialize b)
    => Schema m -- ^ Resolvers.
    -> Maybe Text -- ^ Operation name.
    -> HashMap Name a -- ^ Variable substitution function.
    -> Document -- @GraphQL@ document.
    -> m (Either (ResponseEventStream m b) (Response b))
execute schema' operationName subs document =
    case Transform.document schema' operationName subs document of
        Left queryError -> pure $ singleError $ Transform.queryError queryError
        Right transformed -> executeRequest transformed

executeRequest :: (MonadCatch m, Serialize a)
    => Transform.Document m
    -> m (Either (ResponseEventStream m a) (Response a))
executeRequest (Transform.Document types' rootObjectType operation)
    | (Transform.Query _ fields) <- operation =
        Right <$> executeOperation types' rootObjectType fields
    | (Transform.Mutation _ fields) <- operation =
        Right <$> executeOperation types' rootObjectType fields
    | (Transform.Subscription _ fields) <- operation
        = either singleError Left
        <$> Subscribe.subscribe types' rootObjectType fields

-- This is actually executeMutation, but we don't distinguish between queries
-- and mutations yet.
executeOperation :: (MonadCatch m, Serialize a)
    => HashMap Name (Type m)
    -> Out.ObjectType m
    -> Seq (Transform.Selection m)
    -> m (Response a)
executeOperation types' objectType fields =
    runCollectErrs types' $ executeSelectionSet Definition.Null objectType fields
