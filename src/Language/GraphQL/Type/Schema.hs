{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExplicitForAll #-}

-- | This module provides a representation of a @GraphQL@ Schema in addition to
-- functions for defining and manipulating schemas.
module Language.GraphQL.Type.Schema
    ( Directive(..)
    , Directives
    , Schema(..)
    , Type(..)
    , schema
    ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Language.GraphQL.AST.Document as Full
import Language.GraphQL.AST.DirectiveLocation (DirectiveLocation)
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

-- | Directive definition.
data Directive = Directive (Maybe Text) [DirectiveLocation] In.Arguments

-- | Directive definitions.
type Directives = HashMap Full.Name Directive

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
    , subscription :: Maybe (Out.ObjectType m)
    , directives :: Directives
    }

-- | Shortcut for creating a schema.
schema :: forall m. Out.ObjectType m -> Schema m
schema query' = Schema
    { query = query'
    , mutation = Nothing
    , subscription = Nothing
    , directives = mempty
    }
