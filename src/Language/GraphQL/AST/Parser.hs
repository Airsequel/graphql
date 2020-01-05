{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @GraphQL@ document parser.
module Language.GraphQL.AST.Parser
    ( document
    ) where

import Control.Applicative (Alternative(..), optional)
import qualified Control.Applicative.Combinators.NonEmpty as NonEmpty
import Control.Applicative.Combinators (sepBy)
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
    <?> "Definition"

executableDefinition :: Parser ExecutableDefinition
executableDefinition = DefinitionOperation <$> operationDefinition
    <|> DefinitionFragment  <$> fragmentDefinition
    <?> "ExecutableDefinition"

typeSystemDefinition :: Parser TypeSystemDefinition
typeSystemDefinition = schemaDefinition
    <|> TypeDefinition <$> typeDefinition
    <?> "TypeSystemDefinition"

typeDefinition :: Parser TypeDefinition
typeDefinition = scalarTypeDefinition
    <|> objectTypeDefinition
    <?> "TypeDefinition"

scalarTypeDefinition :: Parser TypeDefinition
scalarTypeDefinition = ScalarTypeDefinition
    <$> description
    <* symbol "scalar"
    <*> name
    <*> opt directives
    <?> "ScalarTypeDefinition"

objectTypeDefinition :: Parser TypeDefinition
objectTypeDefinition = ObjectTypeDefinition
    <$> description
    <* symbol "type"
    <*> name
    <*> opt implementsInterfacesOpt
    <*> opt directives
    <*> braces (many fieldDefinition)
    <?> "ObjectTypeDefinition"

description :: Parser Description
description = Description
    <$> optional (string <|> blockString)
    <?> "Description"

{- TODO:
    implementsInterfaces :: Parser ImplementsInterfaces
implementsInterfaces = ImplementsInterfaces
    <$ symbol "implements"
    <* optional amp
    <*> name `sepBy1` amp
    <?> "ImplementsInterfaces" -}

implementsInterfacesOpt :: Parser ImplementsInterfacesOpt
implementsInterfacesOpt = ImplementsInterfacesOpt
    <$ symbol "implements"
    <* optional amp
    <*> name `sepBy` amp
    <?> "ImplementsInterfaces"

inputValueDefinition :: Parser InputValueDefinition
inputValueDefinition = InputValueDefinition
    <$> description
    <*> name
    <* colon
    <*> type'
    <*> defaultValue
    <*> opt directives
    <?> "InputValueDefinition"

argumentsDefinition :: Parser ArgumentsDefinition
argumentsDefinition = ArgumentsDefinition
    <$> parens (many inputValueDefinition)
    <?> "ArgumentsDefinition"

fieldDefinition :: Parser FieldDefinition
fieldDefinition = FieldDefinition
    <$> description
    <*> name
    <*> opt argumentsDefinition
    <* colon
    <*> type'
    <*> opt directives
    <?> "FieldDefinition"

schemaDefinition :: Parser TypeSystemDefinition
schemaDefinition = SchemaDefinition
    <$ symbol "schema"
    <*> opt directives
    <*> operationTypeDefinitions
    <?> "SchemaDefinition"

operationTypeDefinitions :: Parser OperationTypeDefinitions
operationTypeDefinitions  = braces $ manyNE operationTypeDefinition

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
    <|> Float    <$> try float
    <|> Int      <$> integer
    <|> Boolean  <$> booleanValue
    <|> Null     <$  symbol "null"
    <|> String   <$> blockString
    <|> String   <$> string
    <|> Enum     <$> try enumValue
    <|> List     <$> listValue
    <|> Object   <$> objectValue
    <?> "value error!"
  where
    booleanValue :: Parser Bool
    booleanValue = True  <$ symbol "true"
               <|> False <$ symbol "false"

    enumValue :: Parser Name
    enumValue = but (symbol "true") *> but (symbol "false") *> but (symbol "null") *> name

    listValue :: Parser [Value]
    listValue = brackets $ some value

    objectValue :: Parser [ObjectField]
    objectValue = braces $ some objectField

objectField :: Parser ObjectField
objectField = ObjectField <$> name <* colon <*> value

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

defaultValue :: Parser (Maybe Value)
defaultValue = optional (equals *> value) <?> "DefaultValue"

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
