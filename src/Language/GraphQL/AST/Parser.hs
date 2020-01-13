{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @GraphQL@ document parser.
module Language.GraphQL.AST.Parser
    ( document
    ) where

import Control.Applicative (Alternative(..), optional)
import qualified Control.Applicative.Combinators.NonEmpty as NonEmpty
import Language.GraphQL.AST
import qualified Language.GraphQL.AST.Document as Document
import Language.GraphQL.AST.Lexer
import Text.Megaparsec (lookAhead, option, try, (<?>))

-- | Parser for the GraphQL documents.
document :: Parser Document.Document
document = unicodeBOM
    >> spaceConsumer
    >> lexeme (NonEmpty.some $ Document.ExecutableDefinition <$> definition)

definition :: Parser ExecutableDefinition
definition = DefinitionOperation <$> operationDefinition
         <|> DefinitionFragment  <$> fragmentDefinition
         <?> "definition error!"

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
    <?> "operationType error"

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
objectField = ObjectField <$> name <* symbol ":" <*> value

-- * Variables

variableDefinitions :: Parser [VariableDefinition]
variableDefinitions = listOptIn parens variableDefinition

variableDefinition :: Parser VariableDefinition
variableDefinition = VariableDefinition
    <$> variable
    <*  colon
    <*> type_
    <*> optional defaultValue

variable :: Parser Name
variable = dollar *> name

defaultValue :: Parser Value
defaultValue = equals *> value

-- * Input Types

type_ :: Parser Type
type_ = try (TypeNonNull <$> nonNullType)
    <|> TypeList <$> brackets type_
    <|> TypeNamed <$> name
    <?> "type_ error!"

nonNullType :: Parser NonNullType
nonNullType = NonNullTypeNamed <$> name <* bang
          <|> NonNullTypeList  <$> brackets type_  <* bang
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
