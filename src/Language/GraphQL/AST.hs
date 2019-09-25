-- | This module defines an abstract syntax tree for the @GraphQL@ language based on
--   <https://facebook.github.io/graphql/ Facebook's GraphQL Specification>.
--
-- Target AST for Parser.
module Language.GraphQL.AST
    ( Alias
    , Argument(..)
    , Arguments
    , Definition(..)
    , Directive(..)
    , Directives
    , Document
    , Field(..)
    , FragmentDefinition(..)
    , FragmentName
    , FragmentSpread(..)
    , InlineFragment(..)
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
    , VariableDefinitions
    ) where

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Language.GraphQL.AST.Core ( Alias
                                 , Name
                                 )

-- * Document

-- | GraphQL document.
type Document = NonEmpty Definition

-- * Operations

-- | Top-level definition of a document, either an operation or a fragment.
data Definition = DefinitionOperation OperationDefinition
                | DefinitionFragment  FragmentDefinition
                  deriving (Eq, Show)

-- | Operation definition.
data OperationDefinition = OperationSelectionSet SelectionSet
                         | OperationDefinition   OperationType
                                                 (Maybe Name)
                                                 VariableDefinitions
                                                 Directives
                                                 SelectionSet
                           deriving (Eq, Show)

-- | GraphQL has 3 operation types: queries, mutations and subscribtions.
--
-- Currently only queries and mutations are supported.
data OperationType = Query | Mutation deriving (Eq, Show)

-- * Selections

-- | "Top-level" selection, selection on a operation.
type SelectionSet = NonEmpty Selection

-- | Field selection.
type SelectionSetOpt = [Selection]

-- | Single selection element.
data Selection
    = SelectionField          Field
    | SelectionFragmentSpread FragmentSpread
    | SelectionInlineFragment InlineFragment
    deriving (Eq, Show)

-- * Field

-- | GraphQL field.
data Field
    = Field (Maybe Alias) Name Arguments Directives SelectionSetOpt
    deriving (Eq, Show)

-- * Arguments

-- | Argument list.
type Arguments = [Argument]

-- | Argument.
data Argument = Argument Name Value deriving (Eq,Show)

-- * Fragments

-- | Fragment spread.
data FragmentSpread = FragmentSpread Name Directives deriving (Eq, Show)

-- | Inline fragment.
data InlineFragment = InlineFragment (Maybe TypeCondition) Directives SelectionSet
                      deriving (Eq, Show)

-- | Fragment definition.
data FragmentDefinition
    = FragmentDefinition Name TypeCondition Directives SelectionSet
    deriving (Eq, Show)

{-# DEPRECATED FragmentName "Use Name instead" #-}
type FragmentName = Name

-- | Type condition.
type TypeCondition = Name

-- * Input values

-- | Input value.
data Value = ValueVariable Name
           | ValueInt Int32
           | ValueFloat Double
           | ValueString Text
           | ValueBoolean Bool
           | ValueNull
           | ValueEnum Name
           | ValueList [Value]
           | ValueObject [ObjectField]
           deriving (Eq, Show)

-- | Key-value pair.
--
-- A list of 'ObjectField's represents a GraphQL object type.
data ObjectField = ObjectField Name Value deriving (Eq, Show)

-- * Variables

-- | Variable definition list.
type VariableDefinitions = [VariableDefinition]

-- | Variable definition.
data VariableDefinition = VariableDefinition Name Type (Maybe Value)
                          deriving (Eq, Show)

-- * Input types

-- | Type representation.
data Type = TypeNamed   Name
          | TypeList    Type
          | TypeNonNull NonNullType
            deriving (Eq, Show)


-- | Helper type to represent Non-Null types and lists of such types.
data NonNullType = NonNullTypeNamed Name
                 | NonNullTypeList  Type
                   deriving (Eq, Show)

-- * Directives

-- | Directive list.
type Directives = [Directive]

-- | Directive.
data Directive = Directive Name [Argument] deriving (Eq, Show)
