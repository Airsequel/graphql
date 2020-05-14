module Language.GraphQL.Type.Schema
    ( Schema(..)
    ) where

import Language.GraphQL.Type.Definition

-- | Schema Definition
data Schema m = Schema
    { query :: ObjectType m
    , mutation :: Maybe (ObjectType m)
    }
