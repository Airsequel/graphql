{-# LANGUAGE OverloadedStrings #-}
-- | This module provides the function to execute a @GraphQL@ request --
--   according to a 'Schema'.
module Data.GraphQL.Execute (execute) where

import Control.Monad (MonadPlus(..))
import Data.GraphQL.Error
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Aeson as Aeson
import qualified Data.GraphQL.AST as AST
import qualified Data.GraphQL.AST.Core as AST.Core
import qualified Data.GraphQL.AST.Transform as Transform
import Data.GraphQL.Schema (Schema)
import qualified Data.GraphQL.Schema as Schema

-- | Takes a 'Schema', a variable substitution function ('Schema.Subs'), and a
--   @GraphQL@ 'document'. The substitution is applied to the document using
--  'rootFields', and the 'Schema''s resolvers are applied to the resulting fields.
--
--   Returns the result of the query against the 'Schema' wrapped in a /data/ field, or
--   errors wrapped in an /errors/ field.
execute
  :: (MonadPlus m)
  => Schema m -> Schema.Subs -> AST.Document -> m Aeson.Value
execute schema subs doc = do
    coreDocument <- maybe mzero pure (Transform.document subs doc)
    document schema coreDocument

document :: MonadPlus m => Schema m -> AST.Core.Document -> m Aeson.Value
document schema (op :| []) = operation schema op
document _ _ = error "Multiple operations not supported yet"

operation :: MonadPlus m => Schema m -> AST.Core.Operation -> m Aeson.Value
operation schema (AST.Core.Query flds)
    = runCollectErrs (Schema.resolve (NE.toList schema) (NE.toList flds))
operation schema (AST.Core.Mutation flds)
    = runCollectErrs (Schema.resolve (NE.toList schema) (NE.toList flds))
