-- | This module defines an abstract syntax tree for the @GraphQL@ language. It
-- follows closely the structure given in the specification. Please refer to
-- <https://facebook.github.io/graphql/ Facebook's GraphQL Specification>.
-- for more information.
module Language.GraphQL.AST.Document
    ( Alias
    , Argument(..)
    , Definition(ExecutableDefinition, TypeSystemDefinition)
    , Directive(..)
    , Document
    , ExecutableDefinition(..)
    , FragmentDefinition(..)
    , Name
    , NonNullType(..)
    , ObjectField(..)
    , OperationDefinition(..)
    , OperationType(..)
    , OperationTypeDefinition(..)
    , OperationTypeDefinitions
    , Selection(..)
    , SelectionSet
    , SelectionSetOpt
    , Type(..)
    , TypeCondition
    , TypeSystemDefinition(..)
    , Value(..)
    , VariableDefinition(..)
    ) where

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Language.GraphQL.AST.DirectiveLocation

-- * Language

-- ** Source Text

-- | Name.
type Name = Text

-- ** Document

-- | GraphQL document.
type Document = NonEmpty Definition

-- | All kinds of definitions that can occur in a GraphQL document.
data Definition
    = ExecutableDefinition ExecutableDefinition
    | TypeSystemDefinition TypeSystemDefinition
    | TypeSystemExtension TypeSystemExtension
    deriving (Eq, Show)

-- | Top-level definition of a document, either an operation or a fragment.
data ExecutableDefinition
    = DefinitionOperation OperationDefinition
    | DefinitionFragment FragmentDefinition
    deriving (Eq, Show)

-- ** Operations

-- | Operation definition.
data OperationDefinition
    = SelectionSet SelectionSet
    | OperationDefinition
        OperationType
        (Maybe Name)
        [VariableDefinition]
        [Directive]
        SelectionSet
    deriving (Eq, Show)

-- | GraphQL has 3 operation types:
--
-- * query - a read-only fetch.
-- * mutation - a write operation followed by a fetch.
-- * subscription - a long-lived request that fetches data in response to
-- source events.
--
-- Currently only queries and mutations are supported.
data OperationType = Query | Mutation deriving (Eq, Show)

-- ** Selection Sets

-- | "Top-level" selection, selection on an operation or fragment.
type SelectionSet = NonEmpty Selection

-- | Field selection.
type SelectionSetOpt = [Selection]

-- | Selection is a single entry in a selection set. It can be a single field,
-- fragment spread or inline fragment.
--
-- The only required property of a field is its name. Optionally it can also
-- have an alias, arguments, directives and a list of subfields.
--
-- In the following query "user" is a field with two subfields, "id" and "name":
--
-- @
-- {
--   user {
--     id
--     name
--   }
-- }
-- @
--
-- A fragment spread refers to a fragment defined outside the operation and is
-- expanded at the execution time.
--
-- @
-- {
--   user {
--     ...userFragment
--   }
-- }
--
-- fragment userFragment on UserType {
--   id
--   name
-- }
-- @
--
-- Inline fragments are similar but they don't have any name and the type
-- condition ("on UserType") is optional.
--
-- @
-- {
--   user {
--     ... on UserType {
--       id
--       name
--     }
-- }
-- @
data Selection
    = Field (Maybe Alias) Name [Argument] [Directive] SelectionSetOpt
    | FragmentSpread Name [Directive]
    | InlineFragment (Maybe TypeCondition) [Directive] SelectionSet
    deriving (Eq, Show)

-- ** Arguments

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

-- ** Field Alias

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

-- ** Fragments

-- | Fragment definition.
data FragmentDefinition
    = FragmentDefinition Name TypeCondition [Directive] SelectionSet
    deriving (Eq, Show)

-- | Type condition.
type TypeCondition = Name

-- ** Input Values

-- | Input value.
data Value
    = Variable Name
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
--   A list of 'ObjectField's represents a GraphQL object type.
data ObjectField = ObjectField Name Value deriving (Eq, Show)

-- ** Variables

-- | Variable definition.
data VariableDefinition = VariableDefinition Name Type (Maybe Value)
    deriving (Eq, Show)

-- ** Type References

-- | Type representation.
data Type
    = TypeNamed Name
    | TypeList Type
    | TypeNonNull NonNullType
    deriving (Eq, Show)

type NamedType = Name

-- | Helper type to represent Non-Null types and lists of such types.
data NonNullType
    = NonNullTypeNamed Name
    | NonNullTypeList Type
    deriving (Eq, Show)

-- ** Directives

-- | Directive.
data Directive = Directive Name [Argument] deriving (Eq, Show)

-- * Type System

data TypeSystemDefinition
    = SchemaDefinition [Directive] OperationTypeDefinitions
    | TypeDefinition TypeDefinition
    | DirectiveDefinition
        Description Name ArgumentsDefinition DirectiveLocation
    deriving (Eq, Show)

-- ** Type System Extensions

data TypeSystemExtension
    = SchemaExtension SchemaExtension
    | TypeExtension TypeExtension
    deriving (Eq, Show)

-- ** Schema

type OperationTypeDefinitions = NonEmpty OperationTypeDefinition

data OperationTypeDefinition
    = OperationTypeDefinition OperationType NamedType
    deriving (Eq, Show)

data SchemaExtension
    = SchemaOperationExtension [Directive] OperationTypeDefinitions
    | SchemaDirectiveExtension (NonEmpty Directive)
    deriving (Eq, Show)

-- ** Descriptions

newtype Description = Description (Maybe Text)
    deriving (Eq, Show)

-- ** Types

data TypeDefinition
    = ScalarTypeDefinition Description Name [Directive]
    | ObjectTypeDefinition
        Description Name ImplementsInterfacesOpt [Directive] [FieldDefinition]
    | InterfaceTypeDefinition Description Name [Directive] [FieldDefinition]
    | UnionTypeDefinition Description Name [Directive] UnionMemberTypesOpt
    | EnumTypeDefinition Description Name [Directive] [EnumValueDefinition]
    | InputObjectTypeDefinition
        Description Name [Directive] InputFieldsDefinitionOpt
    deriving (Eq, Show)

data TypeExtension
    = ScalarTypeExtension Name (NonEmpty Directive)
    | ObjectTypeFieldsDefinitionExtension
        Name ImplementsInterfacesOpt [Directive] (NonEmpty FieldDefinition)
    | ObjectTypeDirectivesExtension
        Name ImplementsInterfacesOpt (NonEmpty Directive)
    | ObjectTypeImplementsInterfacesExtension Name ImplementsInterfaces
    | InterfaceTypeFieldsDefinitionExtension
        Name [Directive] (NonEmpty FieldDefinition)
    | InterfaceTypeDirectivesExtension Name (NonEmpty Directive)
    | UnionTypeUnionMemberTypesExtension Name [Directive] UnionMemberTypes
    | UnionDirectivesExtension Name (NonEmpty Directive)
    | EnumTypeEnumValuesDefinitionExtension
        Name [Directive] (NonEmpty EnumValueDefinition)
    | EnumTypeDirectivesExtension Name (NonEmpty Directive)
    | InputObjectTypeInputFieldsDefinitionExtension
        Name [Directive] InputFieldsDefinition
    | InputObjectTypeDirectivesExtension Name (NonEmpty Directive)
    deriving (Eq, Show)

-- ** Objects

newtype ImplementsInterfaces = ImplementsInterfaces (NonEmpty NamedType)
    deriving (Eq, Show)
newtype ImplementsInterfacesOpt = ImplementsInterfacesOpt [NamedType]
    deriving (Eq, Show)

data FieldDefinition
    = FieldDefinition Description Name ArgumentsDefinition Type
    deriving (Eq, Show)

newtype ArgumentsDefinition = ArgumentsDefinition [InputValueDefinition]
    deriving (Eq, Show)

data InputValueDefinition
    = InputValueDefinition Description Name Type (Maybe Value) [Directive]
    deriving (Eq, Show)

-- ** Unions

newtype UnionMemberTypes = UnionMemberTypes (NonEmpty NamedType)
    deriving (Eq, Show)

newtype UnionMemberTypesOpt = UnionMemberTypesOpt [NamedType]
    deriving (Eq, Show)

-- ** Enums

data EnumValueDefinition = EnumValueDefinition Description Name [Directive]
    deriving (Eq, Show)

-- ** Input Objects

newtype InputFieldsDefinition
    = InputFieldsDefinition (NonEmpty InputValueDefinition)
    deriving (Eq, Show)

newtype InputFieldsDefinitionOpt
    = InputFieldsDefinitionOpt [InputValueDefinition]
    deriving (Eq, Show)
