{-# LANGUAGE OverloadedStrings #-}

-- | This module defines an abstract syntax tree for the @GraphQL@ language. It
-- follows closely the structure given in the specification. Please refer to
-- <https://facebook.github.io/graphql/ Facebook's GraphQL Specification>.
-- for more information.
module Language.GraphQL.AST.Document
    ( Alias
    , Argument(..)
    , ArgumentsDefinition(..)
    , ConstValue(..)
    , Definition(..)
    , Description(..)
    , Directive(..)
    , Document
    , EnumValueDefinition(..)
    , ExecutableDefinition(..)
    , FieldDefinition(..)
    , FragmentDefinition(..)
    , ImplementsInterfaces(..)
    , InputValueDefinition(..)
    , Location(..)
    , Name
    , NamedType
    , NonNullType(..)
    , ObjectField(..)
    , OperationDefinition(..)
    , OperationType(..)
    , OperationTypeDefinition(..)
    , SchemaExtension(..)
    , Selection(..)
    , SelectionSet
    , SelectionSetOpt
    , Type(..)
    , TypeCondition
    , TypeDefinition(..)
    , TypeExtension(..)
    , TypeSystemDefinition(..)
    , TypeSystemExtension(..)
    , UnionMemberTypes(..)
    , Value(..)
    , VariableDefinition(..)
    ) where

import Data.Foldable (toList)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.GraphQL.AST.DirectiveLocation

-- * Language

-- ** Source Text

-- | Name.
type Name = Text

-- | Error location, line and column.
data Location = Location
    { line :: Word
    , column :: Word
    } deriving (Eq, Show)

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
data OperationType = Query | Mutation | Subscription deriving (Eq, Show)

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

-- | Input value (literal or variable).
data Value
    = Variable Name
    | Int Int32
    | Float Double
    | String Text
    | Boolean Bool
    | Null
    | Enum Name
    | List [Value]
    | Object [ObjectField Value]
    deriving (Eq, Show)

-- | Constant input value.
data ConstValue
    = ConstInt Int32
    | ConstFloat Double
    | ConstString Text
    | ConstBoolean Bool
    | ConstNull
    | ConstEnum Name
    | ConstList [ConstValue]
    | ConstObject [ObjectField ConstValue]
    deriving (Eq, Show)

-- | Key-value pair.
--
-- A list of 'ObjectField's represents a GraphQL object type.
data ObjectField a = ObjectField Name a
    deriving (Eq, Show)

-- ** Variables

-- | Variable definition.
--
-- Each operation can include a list of variables:
--
-- @
-- query (protagonist: String = "Zarathustra") {
--   getAuthor(protagonist: $protagonist)
-- }
-- @
--
-- This query defines an optional variable @protagonist@ of type @String@,
-- its default value is "Zarathustra". If no default value is defined and no
-- value is provided, a variable can still be @null@ if its type is nullable.
--
-- Variables are usually passed along with the query, but not in the query
-- itself. They make queries reusable.
data VariableDefinition = VariableDefinition Name Type (Maybe ConstValue)
    deriving (Eq, Show)

-- ** Type References

-- | Type representation.
data Type
    = TypeNamed Name
    | TypeList Type
    | TypeNonNull NonNullType
    deriving (Eq, Show)

-- | Represents type names.
type NamedType = Name

-- | Helper type to represent Non-Null types and lists of such types.
data NonNullType
    = NonNullTypeNamed Name
    | NonNullTypeList Type
    deriving (Eq, Show)

-- ** Directives

-- | Directive.
--
-- Directives begin with "@", can accept arguments, and can be applied to the
-- most GraphQL elements, providing additional information.
data Directive = Directive Name [Argument] deriving (Eq, Show)

-- * Type System

-- | Type system can define a schema, a type or a directive.
--
-- @
-- schema {
--   query: Query
-- }
--
-- directive @example on FIELD_DEFINITION
--
-- type Query {
--   field: String @example
-- }
-- @
--
-- This example defines a custom directive "@example", which is applied to a
-- field definition of the type definition "Query". On the top the schema
-- is defined by taking advantage of the type "Query".
data TypeSystemDefinition
    = SchemaDefinition [Directive] (NonEmpty OperationTypeDefinition)
    | TypeDefinition TypeDefinition
    | DirectiveDefinition
        Description Name ArgumentsDefinition (NonEmpty DirectiveLocation)
    deriving (Eq, Show)

-- ** Type System Extensions

-- | Extension for a type system definition. Only schema and type definitions
-- can be extended.
data TypeSystemExtension
    = SchemaExtension SchemaExtension
    | TypeExtension TypeExtension
    deriving (Eq, Show)

-- ** Schema

-- | Root operation type definition.
--
-- Defining root operation types is not required since they have defaults. So
-- the default query root type is "Query", and the default mutation root type
-- is "Mutation". But these defaults can be changed for a specific schema. In
-- the following code the query root type is changed to "MyQueryRootType", and
-- the mutation root type to "MyMutationRootType":
--
-- @
-- schema {
--   query: MyQueryRootType
--   mutation: MyMutationRootType
-- }
-- @
data OperationTypeDefinition
    = OperationTypeDefinition OperationType NamedType
    deriving (Eq, Show)

-- | Extension of the schema definition by further operations or directives.
data SchemaExtension
    = SchemaOperationExtension [Directive] (NonEmpty OperationTypeDefinition)
    | SchemaDirectivesExtension (NonEmpty Directive)
    deriving (Eq, Show)

-- ** Descriptions

-- | GraphQL has built-in capability to document service APIs. Documentation
-- is a GraphQL string that precedes a particular definition and contains
-- Markdown. Any GraphQL definition can be documented this way.
--
-- @
-- """
-- Supported languages.
-- """
-- enum Language {
--   "English"
--   EN
--
--   "Russian"
--   RU
-- }
-- @
newtype Description = Description (Maybe Text)
    deriving (Eq, Show)

-- ** Types

-- | Type definitions describe various user-defined types.
data TypeDefinition
    = ScalarTypeDefinition Description Name [Directive]
    | ObjectTypeDefinition
        Description
        Name
        (ImplementsInterfaces [])
        [Directive]
        [FieldDefinition]
    | InterfaceTypeDefinition Description Name [Directive] [FieldDefinition]
    | UnionTypeDefinition Description Name [Directive] (UnionMemberTypes [])
    | EnumTypeDefinition Description Name [Directive] [EnumValueDefinition]
    | InputObjectTypeDefinition
        Description Name [Directive] [InputValueDefinition]
    deriving (Eq, Show)

-- | Extensions for custom, already defined types.
data TypeExtension
    = ScalarTypeExtension Name (NonEmpty Directive)
    | ObjectTypeFieldsDefinitionExtension
        Name (ImplementsInterfaces []) [Directive] (NonEmpty FieldDefinition)
    | ObjectTypeDirectivesExtension
        Name (ImplementsInterfaces []) (NonEmpty Directive)
    | ObjectTypeImplementsInterfacesExtension
        Name (ImplementsInterfaces NonEmpty)
    | InterfaceTypeFieldsDefinitionExtension
        Name [Directive] (NonEmpty FieldDefinition)
    | InterfaceTypeDirectivesExtension Name (NonEmpty Directive)
    | UnionTypeUnionMemberTypesExtension
        Name [Directive] (UnionMemberTypes NonEmpty)
    | UnionTypeDirectivesExtension Name (NonEmpty Directive)
    | EnumTypeEnumValuesDefinitionExtension
        Name [Directive] (NonEmpty EnumValueDefinition)
    | EnumTypeDirectivesExtension Name (NonEmpty Directive)
    | InputObjectTypeInputFieldsDefinitionExtension
        Name [Directive] (NonEmpty InputValueDefinition)
    | InputObjectTypeDirectivesExtension Name (NonEmpty Directive)
    deriving (Eq, Show)

-- ** Objects

-- | Defines a list of interfaces implemented by the given object type.
--
-- @
-- type Business implements NamedEntity & ValuedEntity {
--   name: String
-- }
-- @
--
-- Here the object type "Business" implements two interfaces: "NamedEntity" and
-- "ValuedEntity".
newtype ImplementsInterfaces t = ImplementsInterfaces (t NamedType)

instance Foldable t => Eq (ImplementsInterfaces t) where
    (ImplementsInterfaces xs) == (ImplementsInterfaces ys)
        = toList xs == toList ys

instance Foldable t => Show (ImplementsInterfaces t) where
    show (ImplementsInterfaces interfaces) = Text.unpack
        $ Text.append "implements"
        $ Text.intercalate " & "
        $ toList interfaces

-- | Definition of a single field in a type.
--
-- @
-- type Person {
--   name: String
--   picture(width: Int, height: Int): Url
-- }
-- @
--
-- "name" and "picture", including their arguments and types, are field
-- definitions.
data FieldDefinition
    = FieldDefinition Description Name ArgumentsDefinition Type [Directive]
    deriving (Eq, Show)

-- | A list of values passed to a field.
--
-- @
-- type Person {
--   name: String
--   picture(width: Int, height: Int): Url
-- }
-- @
--
-- "Person" has two fields, "name" and "picture". "name" doesn't have any
-- arguments, so 'ArgumentsDefinition' contains an empty list. "picture"
-- contains definitions for 2 arguments: "width" and "height".
newtype ArgumentsDefinition = ArgumentsDefinition [InputValueDefinition]
    deriving (Eq, Show)

instance Semigroup ArgumentsDefinition where
    (ArgumentsDefinition xs) <> (ArgumentsDefinition ys) =
        ArgumentsDefinition $ xs <> ys

instance Monoid ArgumentsDefinition where
    mempty = ArgumentsDefinition []

-- | Defines an input value.
--
-- * Input values can define field arguments, see 'ArgumentsDefinition'.
-- * They can also be used as field definitions in an input type.
--
-- @
-- input Point2D {
--   x: Float
--   y: Float
-- }
-- @
--
-- The input type "Point2D" contains two value definitions: "x" and "y".
data InputValueDefinition
    = InputValueDefinition Description Name Type (Maybe ConstValue) [Directive]
    deriving (Eq, Show)

-- ** Unions

-- | List of types forming a union.
--
-- @
-- union SearchResult = Person | Photo
-- @
--
-- "Person" and "Photo" are member types of the union "SearchResult".
newtype UnionMemberTypes t = UnionMemberTypes (t NamedType)

instance Foldable t => Eq (UnionMemberTypes t) where
    (UnionMemberTypes xs) == (UnionMemberTypes ys) = toList xs == toList ys

instance Foldable t => Show (UnionMemberTypes t) where
    show (UnionMemberTypes memberTypes) = Text.unpack
        $ Text.intercalate " | "
        $ toList memberTypes

-- ** Enums

-- | Single value in an enum definition.
--
-- @
-- enum Direction {
--   NORTH
--   EAST
--   SOUTH
--   WEST
-- }
-- @
--
-- "NORTH, "EAST", "SOUTH", and "WEST" are value definitions of an enum type
-- definition "Direction".
data EnumValueDefinition = EnumValueDefinition Description Name [Directive]
    deriving (Eq, Show)
