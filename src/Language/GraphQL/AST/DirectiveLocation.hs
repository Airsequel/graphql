{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

-- | Various parts of a GraphQL document can be annotated with directives. 
-- This module describes locations in a document where directives can appear.
module Language.GraphQL.AST.DirectiveLocation
    ( DirectiveLocation(..)
    , ExecutableDirectiveLocation(..)
    , TypeSystemDirectiveLocation(..)
    ) where

-- | All directives can be splitted in two groups: directives used to annotate
-- various parts of executable definitions and the ones used in the schema
-- definition.
data DirectiveLocation
    = ExecutableDirectiveLocation ExecutableDirectiveLocation
    | TypeSystemDirectiveLocation TypeSystemDirectiveLocation
    deriving (Eq, Show)

-- | Where directives can appear in an executable definition, like a query.
data ExecutableDirectiveLocation
    = Query
    | Mutation
    | Subscription
    | Field
    | FragmentDefinition
    | FragmentSpread
    | InlineFragment
    deriving (Eq, Show)

-- | Where directives can appear in a type system definition.
data TypeSystemDirectiveLocation
    = Schema
    | Scalar
    | Object
    | FieldDefinition
    | ArgumentDefinition
    | Interface
    | Union
    | Enum
    | EnumValue
    | InputObject
    | InputFieldDefinition
    deriving (Eq, Show)
