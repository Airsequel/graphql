-- | This module provides a representation of a @GraphQL@ Schema in addition to
-- functions for defining and manipulating schemas.
module Language.GraphQL.Type.Schema
    ( Schema(..)
    , Type(..)
    ) where

import qualified Language.GraphQL.Type.Definition as Definition
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out

-- | These are all of the possible kinds of types.
data Type m
    = ScalarType Definition.ScalarType
    | EnumType Definition.EnumType
    | ObjectType (Out.ObjectType m)
    | InputObjectType In.InputObjectType
    | InterfaceType (Out.InterfaceType m)
    | UnionType (Out.UnionType m)
    deriving Eq

-- | A Schema is created by supplying the root types of each type of operation,
--   query and mutation (optional). A schema definition is then supplied to the
--   validator and executor.
--
--   __Note:__ When the schema is constructed, by default only the types that
--   are reachable by traversing the root types are included, other types must
--   be explicitly referenced.
data Schema m = Schema
    { query :: Out.ObjectType m
    , mutation :: Maybe (Out.ObjectType m)
    }
