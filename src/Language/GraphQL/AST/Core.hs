-- | This is the AST meant to be executed.
module Language.GraphQL.AST.Core
    ( Alias
    , Argument(..)
    , Document
    , Field(..)
    , Fragment(..)
    , Name
    , ObjectField(..)
    , Operation(..)
    , Selection(..)
    , TypeCondition
    , Value(..)
    ) where

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.String
import Data.Text (Text)

-- | Name
type Name = Text

-- | GraphQL document is a non-empty list of operations.
type Document = NonEmpty Operation

-- | GraphQL has 3 operation types: queries, mutations and subscribtions.
--
-- Currently only queries and mutations are supported.
data Operation
    = Query (Maybe Text) (NonEmpty Selection)
    | Mutation (Maybe Text) (NonEmpty Selection)
    deriving (Eq, Show)

-- | A single GraphQL field.
--
-- Only required property of a field, is its name. Optionally it can also have
-- an alias, arguments or a list of subfields.
--
-- Given the following query:
--
-- @
-- {
--   zuck: user(id: 4) {
--     id
--     name
--   }
-- }
-- @
--
-- * "user", "id" and "name" are field names.
-- * "user" has two subfields, "id" and "name".
-- * "zuck" is an alias for "user". "id" and "name" have no aliases.
-- * "id: 4" is an argument for "name". "id" and "name don't have any
-- arguments.
data Field = Field (Maybe Alias) Name [Argument] [Selection] deriving (Eq, Show)

-- | Alternative field name.
--
-- @
-- {
--   smallPic: profilePic(size: 64)
--   bigPic: profilePic(size: 1024)
-- }
-- @
--
-- Here "smallPic" and "bigPic" are aliases for the same field, "profilePic",
-- used to distinquish between profile pictures with different arguments
-- (sizes).
type Alias = Name

-- | Single argument.
--
-- @
-- {
--   user(id: 4) {
--     name
--   }
-- }
-- @
--
--  Here "id" is an argument for the field "user" and its value is 4.
data Argument = Argument Name Value deriving (Eq, Show)

-- | Represents accordingly typed GraphQL values.
data Value
    = ValueInt Int32
    -- GraphQL Float is double precision
    | ValueFloat Double
    | ValueString Text
    | ValueBoolean Bool
    | ValueNull
    | ValueEnum Name
    | ValueList [Value]
    | ValueObject [ObjectField]
    deriving (Eq, Show)

instance IsString Value where
  fromString = ValueString . fromString

-- | Key-value pair.
--
-- A list of 'ObjectField's represents a GraphQL object type.
data ObjectField = ObjectField Name Value deriving (Eq, Show)

-- | Type condition.
type TypeCondition = Name

-- | Represents fragments and inline fragments.
data Fragment
    = Fragment TypeCondition (NonEmpty Selection)
    deriving (Eq, Show)

-- | Single selection element.
data Selection
    = SelectionFragment Fragment
    | SelectionField Field
    deriving (Eq, Show)
