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

type Document = NonEmpty Definition

-- * Operations

data Definition = DefinitionOperation OperationDefinition
                | DefinitionFragment  FragmentDefinition
                  deriving (Eq,Show)

data OperationDefinition = OperationSelectionSet SelectionSet
                         | OperationDefinition   OperationType
                                                 (Maybe Name)
                                                 VariableDefinitions
                                                 Directives
                                                 SelectionSet
                           deriving (Eq,Show)

data OperationType = Query | Mutation deriving (Eq,Show)

-- * SelectionSet

type SelectionSet = NonEmpty Selection

type SelectionSetOpt = [Selection]

data Selection = SelectionField          Field
               | SelectionFragmentSpread FragmentSpread
               | SelectionInlineFragment InlineFragment
                 deriving (Eq,Show)

-- * Field

data Field = Field (Maybe Alias) Name Arguments Directives SelectionSetOpt
             deriving (Eq,Show)

-- * Arguments

type Arguments = [Argument]

data Argument = Argument Name Value deriving (Eq,Show)

-- * Fragments

data FragmentSpread = FragmentSpread Name Directives deriving (Eq,Show)

data InlineFragment = InlineFragment (Maybe TypeCondition) Directives SelectionSet
                      deriving (Eq,Show)

data FragmentDefinition =
  FragmentDefinition FragmentName TypeCondition Directives SelectionSet
  deriving (Eq,Show)

type FragmentName = Name

type TypeCondition = Name

-- * Input values

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

data ObjectField = ObjectField Name Value deriving (Eq, Show)

-- * Variables

type VariableDefinitions = [VariableDefinition]

data VariableDefinition = VariableDefinition Name Type (Maybe Value)
                          deriving (Eq,Show)

-- * Input types

data Type = TypeNamed   Name
          | TypeList    Type
          | TypeNonNull NonNullType
            deriving (Eq,Show)

data NonNullType = NonNullTypeNamed Name
                 | NonNullTypeList  Type
                   deriving (Eq,Show)

-- * Directives

type Directives = [Directive]

data Directive = Directive Name [Argument] deriving (Eq,Show)
