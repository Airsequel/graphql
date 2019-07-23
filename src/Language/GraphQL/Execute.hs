{-# LANGUAGE OverloadedStrings #-}

-- | This module provides the function to execute a @GraphQL@ request --
--   according to a 'Schema'.
module Language.GraphQL.Execute
    ( execute
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Aeson as Aeson
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
execute
    :: MonadIO m
    => Schema m -> Schema.Subs -> AST.Document -> m Aeson.Value
execute schema subs doc =
    maybe transformError (document schema) $ Transform.document subs doc
  where
    transformError = return $ singleError "Schema transformation error."

document :: MonadIO m => Schema m -> AST.Core.Document -> m Aeson.Value
document schema (op :| []) = operation schema op
document _ _ = return $ singleError "Multiple operations not supported yet."

operation :: MonadIO m => Schema m -> AST.Core.Operation -> m Aeson.Value
operation schema (AST.Core.Query flds)
    = runCollectErrs (Schema.resolve (NE.toList schema) (NE.toList flds))
operation schema (AST.Core.Mutation flds)
    = runCollectErrs (Schema.resolve (NE.toList schema) (NE.toList flds))
