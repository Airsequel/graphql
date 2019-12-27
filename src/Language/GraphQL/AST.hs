-- | Target AST for Parser.
module Language.GraphQL.AST
    ( Alias
    , Argument(..)
    , ExecutableDefinition(..)
    , Directive(..)
    , FragmentDefinition(..)
    , Name
    , NonNullType(..)
    , ObjectField(..)
    , OperationDefinition(..)
    , OperationType(..)
    , Selection(..)
    , SelectionSet
    , SelectionSetOpt
    , Type(..)
    , TypeCondition
    , Value(..)
    , VariableDefinition(..)
    ) where

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

-- | Name
type Name = Text

-- | Directive.
data Directive = Directive Name [Argument] deriving (Eq, Show)

-- * Operations

-- | Top-level definition of a document, either an operation or a fragment.
data ExecutableDefinition
    = DefinitionOperation OperationDefinition
    | DefinitionFragment FragmentDefinition
    deriving (Eq, Show)

-- | Operation definition.
data OperationDefinition
    = OperationSelectionSet SelectionSet
    | OperationDefinition OperationType
                          (Maybe Name)
                          [VariableDefinition]
                          [Directive]
                          SelectionSet
    deriving (Eq, Show)

-- | GraphQL has 3 operation types: queries, mutations and subscribtions.
--
-- Currently only queries and mutations are supported.
data OperationType = Query | Mutation deriving (Eq, Show)

-- * Selections

-- | "Top-level" selection, selection on an operation or fragment.
type SelectionSet = NonEmpty Selection

-- | Field selection.
type SelectionSetOpt = [Selection]

-- | Single GraphQL field.
--
-- The only required property of a field is its name. Optionally it can also
-- have an alias, arguments or a list of subfields.
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
-- * "id: 4" is an argument for "user". "id" and "name" don't have any
-- arguments.
data Selection
    = Field (Maybe Alias) Name [Argument] [Directive] SelectionSetOpt
    | FragmentSpread Name [Directive]
    | InlineFragment (Maybe TypeCondition) [Directive] SelectionSet
    deriving (Eq, Show)

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
data Argument = Argument Name Value deriving (Eq,Show)

-- | Fragment definition.
data FragmentDefinition
    = FragmentDefinition Name TypeCondition [Directive] SelectionSet
    deriving (Eq, Show)

-- * Inputs

-- | Input value.
data Value = Variable Name
           | Int Int32
           | Float Double
           | String Text
           | Boolean Bool
           | Null
           | Enum Name
           | List [Value]
           | Object [ObjectField]
           deriving (Eq, Show)

-- | Key-value pair.
--
-- A list of 'ObjectField's represents a GraphQL object type.
data ObjectField = ObjectField Name Value deriving (Eq, Show)

-- | Variable definition.
data VariableDefinition = VariableDefinition Name Type (Maybe Value)
                          deriving (Eq, Show)

-- | Type condition.
type TypeCondition = Name

-- | Type representation.
data Type = TypeNamed   Name
          | TypeList    Type
          | TypeNonNull NonNullType
            deriving (Eq, Show)

-- | Helper type to represent Non-Null types and lists of such types.
data NonNullType = NonNullTypeNamed Name
                 | NonNullTypeList  Type
                   deriving (Eq, Show)
