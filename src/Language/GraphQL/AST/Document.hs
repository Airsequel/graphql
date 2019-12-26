-- | This module defines data structures representing a GraphQL document.
module Language.GraphQL.AST.Document
    ( Definition(..)
    , Document
    , ExecutableDefinition(..)
    ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Language.GraphQL.AST
    ( ExecutableDefinition(..)
    , Directive
    , Name
    , OperationType
    , Type
    , Value
    )
import Language.GraphQL.AST.DirectiveLocation

-- | GraphQL document.
type Document = NonEmpty Definition

type NamedType = Name

newtype Description = Description (Maybe Text)
    deriving (Eq, Show)

type RootOperationTypeDefinitions = NonEmpty RootOperationTypeDefinition

data RootOperationTypeDefinition
    = RootOperationTypeDefinition OperationType NamedType
    deriving (Eq, Show)

-- | All kinds of definitions that can occur in a GraphQL document.
data Definition
    = ExecutableDefinition ExecutableDefinition
    | TypeSystemDefinition TypeSystemDefinition
    | TypeSystemExtension TypeSystemExtension
    deriving (Eq, Show)

data TypeSystemDefinition
    = SchemaDefinition [Directive] RootOperationTypeDefinitions
    | TypeDefinition TypeDefinition
    | DirectiveDefinition Description Name ArgumentsDefinition DirectiveLocation
    deriving (Eq, Show)

data SchemaExtension
    = SchemaOperationExtension [Directive] RootOperationTypeDefinitions
    | SchemaDirectiveExtension (NonEmpty Directive)
    deriving (Eq, Show)

data TypeSystemExtension
    = SchemaExtension SchemaExtension
    | TypeExtension TypeExtension
    deriving (Eq, Show)

newtype ImplementsInterfaces = ImplementsInterfaces (NonEmpty NamedType)
    deriving (Eq, Show)
newtype ImplementsInterfacesOpt = ImplementsInterfacesOpt [NamedType]
    deriving (Eq, Show)

newtype UnionMemberTypes = UnionMemberTypes (NonEmpty NamedType)
    deriving (Eq, Show)
newtype UnionMemberTypesOpt = UnionMemberTypesOpt [NamedType]
    deriving (Eq, Show)

newtype InputFieldsDefinition = InputFieldsDefinition (NonEmpty InputValueDefinition)
    deriving (Eq, Show)
newtype InputFieldsDefinitionOpt = InputFieldsDefinitionOpt [InputValueDefinition]
    deriving (Eq, Show)

data InputValueDefinition
    = InputValueDefinition Description Name Type (Maybe Value) [Directive]
    deriving (Eq, Show)

newtype ArgumentsDefinition = ArgumentsDefinition [InputValueDefinition]
    deriving (Eq, Show)

data EnumValueDefinition = EnumValueDefinition Description Name [Directive]
    deriving (Eq, Show)

data FieldDefinition = FieldDefinition Description Name ArgumentsDefinition Type
    deriving (Eq, Show)

data TypeDefinition
    = ScalarTypeDefinition Description Name [Directive]
    | ObjectTypeDefinition Description Name ImplementsInterfacesOpt [Directive] [FieldDefinition]
    | InterfaceTypeDefinition Description Name [Directive] [FieldDefinition]
    | UnionTypeDefinition Description Name [Directive] UnionMemberTypesOpt
    | EnumTypeDefinition Description Name [Directive] [EnumValueDefinition]
    | InputObjectTypeDefinition Description Name [Directive] InputFieldsDefinitionOpt
    deriving (Eq, Show)

data TypeExtension
    = ScalarTypeExtension Name (NonEmpty Directive)
    | ObjectTypeFieldsDefinitionExtension Name ImplementsInterfacesOpt [Directive] (NonEmpty FieldDefinition)
    | ObjectTypeDirectivesExtension Name ImplementsInterfacesOpt (NonEmpty Directive)
    | ObjectTypeImplementsInterfacesExtension Name ImplementsInterfaces
    | InterfaceTypeFieldsDefinitionExtension Name [Directive] (NonEmpty FieldDefinition)
    | InterfaceTypeDirectivesExtension Name (NonEmpty Directive)
    | UnionTypeUnionMemberTypesExtension Name [Directive] UnionMemberTypes
    | UnionDirectivesExtension Name (NonEmpty Directive)
    | EnumTypeEnumValuesDefinitionExtension Name [Directive] (NonEmpty EnumValueDefinition)
    | EnumTypeDirectivesExtension Name (NonEmpty Directive)
    | InputObjectTypeInputFieldsDefinitionExtension Name [Directive] InputFieldsDefinition
    | InputObjectTypeDirectivesExtension Name (NonEmpty Directive)
    deriving (Eq, Show)
