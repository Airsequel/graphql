{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @GraphQL@ document parser.
module Language.GraphQL.AST.Parser
    ( document
    ) where

import Control.Applicative (Alternative(..), optional)
import Control.Applicative.Combinators (sepBy1)
import qualified Control.Applicative.Combinators.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Language.GraphQL.AST.DirectiveLocation as Directive
import Language.GraphQL.AST.DirectiveLocation
    ( DirectiveLocation
    , ExecutableDirectiveLocation
    , TypeSystemDirectiveLocation
    )
import Language.GraphQL.AST.Document
import Language.GraphQL.AST.Lexer
import Text.Megaparsec (lookAhead, option, try, (<?>))

-- | Parser for the GraphQL documents.
document :: Parser Document
document = unicodeBOM
    >> spaceConsumer
    >> lexeme (NonEmpty.some definition)

definition :: Parser Definition
definition = ExecutableDefinition <$> executableDefinition
    <|> TypeSystemDefinition <$> typeSystemDefinition
    <|> TypeSystemExtension <$> typeSystemExtension
    <?> "Definition"

executableDefinition :: Parser ExecutableDefinition
executableDefinition = DefinitionOperation <$> operationDefinition
    <|> DefinitionFragment  <$> fragmentDefinition
    <?> "ExecutableDefinition"

typeSystemDefinition :: Parser TypeSystemDefinition
typeSystemDefinition = schemaDefinition
    <|> TypeDefinition <$> typeDefinition
    <|> directiveDefinition
    <?> "TypeSystemDefinition"

typeSystemExtension :: Parser TypeSystemExtension
typeSystemExtension = SchemaExtension <$> schemaExtension
    <|> TypeExtension <$> typeExtension
    <?> "TypeSystemExtension"

directiveDefinition :: Parser TypeSystemDefinition
directiveDefinition = DirectiveDefinition
    <$> description
    <* symbol "directive"
    <* at
    <*> name
    <*> argumentsDefinition
    <* symbol "on"
    <*> directiveLocations
    <?> "DirectiveDefinition"

directiveLocations :: Parser (NonEmpty DirectiveLocation)
directiveLocations = optional pipe
    *> directiveLocation `NonEmpty.sepBy1` pipe

directiveLocation :: Parser DirectiveLocation
directiveLocation
    = Directive.ExecutableDirectiveLocation <$> executableDirectiveLocation
    <|> Directive.TypeSystemDirectiveLocation <$> typeSystemDirectiveLocation

executableDirectiveLocation :: Parser ExecutableDirectiveLocation
executableDirectiveLocation = Directive.Query <$ symbol "QUERY"
    <|> Directive.Mutation <$ symbol "MUTATION"
    <|> Directive.Subscription <$ symbol "SUBSCRIPTION"
    <|> Directive.Field <$ symbol "FIELD"
    <|> Directive.FragmentDefinition <$ "FRAGMENT_DEFINITION"
    <|> Directive.FragmentSpread <$ "FRAGMENT_SPREAD"
    <|> Directive.InlineFragment <$ "INLINE_FRAGMENT"

typeSystemDirectiveLocation :: Parser TypeSystemDirectiveLocation
typeSystemDirectiveLocation = Directive.Schema <$ symbol "SCHEMA"
    <|> Directive.Scalar <$ symbol "SCALAR"
    <|> Directive.Object <$ symbol "OBJECT"
    <|> Directive.FieldDefinition <$ symbol "FIELD_DEFINITION"
    <|> Directive.ArgumentDefinition <$ symbol "ARGUMENT_DEFINITION"
    <|> Directive.Interface <$ symbol "INTERFACE"
    <|> Directive.Union <$ symbol "UNION"
    <|> Directive.Enum <$ symbol "ENUM"
    <|> Directive.EnumValue <$ symbol "ENUM_VALUE"
    <|> Directive.InputObject <$ symbol "INPUT_OBJECT"
    <|> Directive.InputFieldDefinition <$ symbol "INPUT_FIELD_DEFINITION"

typeDefinition :: Parser TypeDefinition
typeDefinition = scalarTypeDefinition
    <|> objectTypeDefinition
    <|> interfaceTypeDefinition
    <|> unionTypeDefinition
    <|> enumTypeDefinition
    <|> inputObjectTypeDefinition
    <?> "TypeDefinition"

typeExtension :: Parser TypeExtension
typeExtension = scalarTypeExtension
    <|> objectTypeExtension
    <|> interfaceTypeExtension
    <|> unionTypeExtension
    <|> enumTypeExtension
    <|> inputObjectTypeExtension
    <?> "TypeExtension"

scalarTypeDefinition :: Parser TypeDefinition
scalarTypeDefinition = ScalarTypeDefinition
    <$> description
    <* symbol "scalar"
    <*> name
    <*> directives
    <?> "ScalarTypeDefinition"

scalarTypeExtension :: Parser TypeExtension
scalarTypeExtension = extend "scalar" "ScalarTypeExtension"
    $ (ScalarTypeExtension <$> name <*> NonEmpty.some directive) :| []

objectTypeDefinition :: Parser TypeDefinition
objectTypeDefinition = ObjectTypeDefinition
    <$> description
    <* symbol "type"
    <*> name
    <*> option (ImplementsInterfaces []) (implementsInterfaces sepBy1)
    <*> directives
    <*> braces (many fieldDefinition)
    <?> "ObjectTypeDefinition"

objectTypeExtension :: Parser TypeExtension
objectTypeExtension = extend "type" "ObjectTypeExtension"
    $ fieldsDefinitionExtension :|
        [ directivesExtension
        , implementsInterfacesExtension
        ]
  where
    fieldsDefinitionExtension = ObjectTypeFieldsDefinitionExtension
        <$> name
        <*> option (ImplementsInterfaces []) (implementsInterfaces sepBy1)
        <*> directives
        <*> braces (NonEmpty.some fieldDefinition)
    directivesExtension = ObjectTypeDirectivesExtension
        <$> name
        <*> option (ImplementsInterfaces []) (implementsInterfaces sepBy1)
        <*> NonEmpty.some directive
    implementsInterfacesExtension = ObjectTypeImplementsInterfacesExtension
        <$> name
        <*> implementsInterfaces NonEmpty.sepBy1

description :: Parser Description
description = Description
    <$> optional (string <|> blockString)
    <?> "Description"

unionTypeDefinition :: Parser TypeDefinition
unionTypeDefinition = UnionTypeDefinition
    <$> description
    <* symbol "union"
    <*> name
    <*> directives
    <*> option (UnionMemberTypes []) (unionMemberTypes sepBy1)
    <?> "UnionTypeDefinition"

unionTypeExtension :: Parser TypeExtension
unionTypeExtension = extend "union" "UnionTypeExtension"
    $ unionMemberTypesExtension :| [directivesExtension]
  where
    unionMemberTypesExtension = UnionTypeUnionMemberTypesExtension
        <$> name
        <*> directives
        <*> unionMemberTypes NonEmpty.sepBy1
    directivesExtension = UnionTypeDirectivesExtension
        <$> name
        <*> NonEmpty.some directive

unionMemberTypes ::
    Foldable t =>
    (Parser Text -> Parser Text -> Parser (t NamedType)) ->
    Parser (UnionMemberTypes t)
unionMemberTypes sepBy' = UnionMemberTypes
    <$ equals
    <* optional pipe
    <*> name `sepBy'` pipe
    <?> "UnionMemberTypes"

interfaceTypeDefinition :: Parser TypeDefinition
interfaceTypeDefinition = InterfaceTypeDefinition
    <$> description
    <* symbol "interface"
    <*> name
    <*> directives
    <*> braces (many fieldDefinition)
    <?> "InterfaceTypeDefinition"

interfaceTypeExtension :: Parser TypeExtension
interfaceTypeExtension = extend "interface" "InterfaceTypeExtension"
    $ fieldsDefinitionExtension :| [directivesExtension]
  where
    fieldsDefinitionExtension = InterfaceTypeFieldsDefinitionExtension
        <$> name
        <*> directives
        <*> braces (NonEmpty.some fieldDefinition)
    directivesExtension = InterfaceTypeDirectivesExtension
        <$> name
        <*> NonEmpty.some directive

enumTypeDefinition :: Parser TypeDefinition
enumTypeDefinition = EnumTypeDefinition
    <$> description
    <* symbol "enum"
    <*> name
    <*> directives
    <*> listOptIn braces enumValueDefinition
    <?> "EnumTypeDefinition"

enumTypeExtension :: Parser TypeExtension
enumTypeExtension = extend "enum" "EnumTypeExtension"
    $ enumValuesDefinitionExtension :| [directivesExtension]
  where
    enumValuesDefinitionExtension = EnumTypeEnumValuesDefinitionExtension
        <$> name
        <*> directives
        <*> braces (NonEmpty.some enumValueDefinition)
    directivesExtension = EnumTypeDirectivesExtension
        <$> name
        <*> NonEmpty.some directive

inputObjectTypeDefinition :: Parser TypeDefinition
inputObjectTypeDefinition = InputObjectTypeDefinition
    <$> description
    <* symbol "input"
    <*> name
    <*> directives
    <*> listOptIn braces inputValueDefinition
    <?> "InputObjectTypeDefinition"

inputObjectTypeExtension :: Parser TypeExtension
inputObjectTypeExtension = extend "input" "InputObjectTypeExtension"
    $ inputFieldsDefinitionExtension :| [directivesExtension]
  where
    inputFieldsDefinitionExtension = InputObjectTypeInputFieldsDefinitionExtension
        <$> name
        <*> directives
        <*> braces (NonEmpty.some inputValueDefinition)
    directivesExtension = InputObjectTypeDirectivesExtension
        <$> name
        <*> NonEmpty.some directive

enumValueDefinition :: Parser EnumValueDefinition
enumValueDefinition = EnumValueDefinition
    <$> description
    <*> enumValue
    <*> directives
    <?> "EnumValueDefinition"

implementsInterfaces ::
    Foldable t =>
    (Parser Text -> Parser Text -> Parser (t NamedType)) ->
    Parser (ImplementsInterfaces t)
implementsInterfaces sepBy' = ImplementsInterfaces
    <$ symbol "implements"
    <* optional amp
    <*> name `sepBy'` amp
    <?> "ImplementsInterfaces"

inputValueDefinition :: Parser InputValueDefinition
inputValueDefinition = InputValueDefinition
    <$> description
    <*> name
    <* colon
    <*> type'
    <*> defaultValue
    <*> directives
    <?> "InputValueDefinition"

argumentsDefinition :: Parser ArgumentsDefinition
argumentsDefinition = ArgumentsDefinition
    <$> listOptIn parens inputValueDefinition
    <?> "ArgumentsDefinition"

fieldDefinition :: Parser FieldDefinition
fieldDefinition = FieldDefinition
    <$> description
    <*> name
    <*> argumentsDefinition
    <* colon
    <*> type'
    <*> directives
    <?> "FieldDefinition"

schemaDefinition :: Parser TypeSystemDefinition
schemaDefinition = SchemaDefinition
    <$ symbol "schema"
    <*> directives
    <*> operationTypeDefinitions
    <?> "SchemaDefinition"

operationTypeDefinitions :: Parser (NonEmpty OperationTypeDefinition)
operationTypeDefinitions = braces $ NonEmpty.some operationTypeDefinition

schemaExtension :: Parser SchemaExtension
schemaExtension = extend "schema" "SchemaExtension"
    $ schemaOperationExtension :| [directivesExtension]
  where
    directivesExtension = SchemaDirectivesExtension
        <$> NonEmpty.some directive
    schemaOperationExtension = SchemaOperationExtension
        <$> directives
        <*> operationTypeDefinitions

operationTypeDefinition :: Parser OperationTypeDefinition
operationTypeDefinition = OperationTypeDefinition
    <$> operationType <* colon
    <*> name
    <?> "OperationTypeDefinition"

operationDefinition :: Parser OperationDefinition
operationDefinition = SelectionSet <$> selectionSet
    <|> operationDefinition'
    <?> "operationDefinition error"
  where
    operationDefinition'
        = OperationDefinition <$> operationType
        <*> optional name
        <*> variableDefinitions
        <*> directives
        <*> selectionSet

operationType :: Parser OperationType
operationType = Query <$ symbol "query"
    <|> Mutation <$ symbol "mutation"
    -- <?> Keep default error message

-- * SelectionSet

selectionSet :: Parser SelectionSet
selectionSet = braces $ NonEmpty.some selection

selectionSetOpt :: Parser SelectionSetOpt
selectionSetOpt = listOptIn braces selection

selection :: Parser Selection
selection = field
    <|> try fragmentSpread
    <|> inlineFragment
    <?> "selection error!"

-- * Field

field :: Parser Selection
field = Field
    <$> optional alias
    <*> name
    <*> arguments
    <*> directives
    <*> selectionSetOpt

alias :: Parser Alias
alias = try $ name <* colon

-- * Arguments

arguments :: Parser [Argument]
arguments = listOptIn parens argument

argument :: Parser Argument
argument = Argument <$> name <* colon <*> value

-- * Fragments

fragmentSpread :: Parser Selection
fragmentSpread = FragmentSpread
    <$ spread
    <*> fragmentName
    <*> directives

inlineFragment :: Parser Selection
inlineFragment = InlineFragment
    <$ spread
    <*> optional typeCondition
    <*> directives
    <*> selectionSet

fragmentDefinition :: Parser FragmentDefinition
fragmentDefinition = FragmentDefinition
                 <$  symbol "fragment"
                 <*> name
                 <*> typeCondition
                 <*> directives
                 <*> selectionSet

fragmentName :: Parser Name
fragmentName = but (symbol "on") *> name

typeCondition :: Parser TypeCondition
typeCondition = symbol "on" *> name

-- * Input Values

value :: Parser Value
value = Variable <$> variable
    <|> Float <$> try float
    <|> Int <$> integer
    <|> Boolean <$> booleanValue
    <|> Null <$  symbol "null"
    <|> String <$> blockString
    <|> String <$> string
    <|> Enum <$> try enumValue
    <|> List <$> brackets (some value)
    <|> Object <$> braces (some $ objectField value)
    <?> "value error!"

constValue :: Parser ConstValue
constValue = ConstFloat <$> try float
    <|> ConstInt <$> integer
    <|> ConstBoolean <$> booleanValue
    <|> ConstNull <$  symbol "null"
    <|> ConstString <$> blockString
    <|> ConstString <$> string
    <|> ConstEnum <$> try enumValue
    <|> ConstList <$> brackets (some constValue)
    <|> ConstObject <$> braces (some $ objectField constValue)
    <?> "value error!"

booleanValue :: Parser Bool
booleanValue = True  <$ symbol "true"
    <|> False <$ symbol "false"

enumValue :: Parser Name
enumValue = but (symbol "true") *> but (symbol "false") *> but (symbol "null") *> name

objectField :: Parser a -> Parser (ObjectField a)
objectField valueParser = ObjectField <$> name <* colon <*> valueParser

-- * Variables

variableDefinitions :: Parser [VariableDefinition]
variableDefinitions = listOptIn parens variableDefinition

variableDefinition :: Parser VariableDefinition
variableDefinition = VariableDefinition
    <$> variable
    <*  colon
    <*> type'
    <*> defaultValue
    <?> "VariableDefinition"

variable :: Parser Name
variable = dollar *> name

defaultValue :: Parser (Maybe ConstValue)
defaultValue = optional (equals *> constValue) <?> "DefaultValue"

-- * Input Types

type' :: Parser Type
type' = try (TypeNonNull <$> nonNullType)
    <|> TypeList <$> brackets type'
    <|> TypeNamed <$> name
    <?> "Type"

nonNullType :: Parser NonNullType
nonNullType = NonNullTypeNamed <$> name <* bang
          <|> NonNullTypeList  <$> brackets type'  <* bang
          <?> "nonNullType error!"

-- * Directives

directives :: Parser [Directive]
directives = many directive

directive :: Parser Directive
directive = Directive
    <$  at
    <*> name
    <*> arguments

-- * Internal

listOptIn :: (Parser [a] -> Parser [a]) -> Parser a -> Parser [a]
listOptIn surround = option [] . surround . some

-- Hack to reverse parser success
but :: Parser a -> Parser ()
but pn = False <$ lookAhead pn <|> pure True >>= \case
    False -> empty
    True  -> pure ()
