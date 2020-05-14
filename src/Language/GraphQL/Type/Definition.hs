module Language.GraphQL.Type.Definition
    ( ObjectType(..)
    ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Language.GraphQL.Schema

type Fields m = HashMap Text (FieldResolver m)

-- | Object Type Definition.
--
--   Almost all of the GraphQL types you define will be object types. Object
--   types have a name, but most importantly describe their fields.
data ObjectType m = ObjectType
    { name :: Text
    , fields :: Fields m
    }
