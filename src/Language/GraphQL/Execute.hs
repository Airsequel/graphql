{-# LANGUAGE OverloadedStrings #-}

-- | This module provides the function to execute a @GraphQL@ request --
--   according to a 'Schema'.
module Language.GraphQL.Execute
    ( execute
    , executeWithName
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.GraphQL.AST as AST
import qualified Language.GraphQL.AST.Core as AST.Core
import qualified Language.GraphQL.AST.Transform as Transform
import Language.GraphQL.Error
import Language.GraphQL.Schema (Schema)
import qualified Language.GraphQL.Schema as Schema

-- | Takes a 'Schema', a variable substitution function ('Schema.Subs'), and a
--   @GraphQL@ 'document'. The substitution is applied to the document using
--  'rootFields', and the 'Schema''s resolvers are applied to the resulting fields.
--
--   Returns the result of the query against the 'Schema' wrapped in a /data/ field, or
--   errors wrapped in an /errors/ field.
execute :: MonadIO m
    => Schema m
    -> Schema.Subs
    -> AST.Document
    -> m Aeson.Value
execute schema subs doc =
    maybe transformError (document schema Nothing) $ Transform.document subs doc
  where
    transformError = return $ singleError "Schema transformation error."

-- | Takes a 'Schema', operation name, a variable substitution function ('Schema.Subs'),
--   and a @GraphQL@ 'document'. The substitution is applied to the document using
--  'rootFields', and the 'Schema''s resolvers are applied to the resulting fields.
--
--   Returns the result of the query against the 'Schema' wrapped in a /data/ field, or
--   errors wrapped in an /errors/ field.
executeWithName :: MonadIO m
    => Schema m
    -> Text
    -> Schema.Subs
    -> AST.Document
    -> m Aeson.Value
executeWithName schema name subs doc =
    maybe transformError (document schema $ Just name) $ Transform.document subs doc
  where
    transformError = return $ singleError "Schema transformation error."

document :: MonadIO m => Schema m -> Maybe Text -> AST.Core.Document -> m Aeson.Value
document schema Nothing (op :| []) = operation schema op
document schema (Just name) operations = case NE.dropWhile matchingName operations of
    [] -> return $ singleError
        $ Text.unwords ["Operation", name, "couldn't be found in the document."]
    (op:_)  -> operation schema op
  where
    matchingName (AST.Core.Query (Just name') _) = name == name'
    matchingName (AST.Core.Mutation (Just name') _) = name == name'
    matchingName _ = False
document _ _ _ = return $ singleError "Missing operation name."

operation :: MonadIO m => Schema m -> AST.Core.Operation -> m Aeson.Value
operation schema (AST.Core.Query _ flds)
    = runCollectErrs (Schema.resolve (NE.toList schema) (NE.toList flds))
operation schema (AST.Core.Mutation _ flds)
    = runCollectErrs (Schema.resolve (NE.toList schema) (NE.toList flds))
