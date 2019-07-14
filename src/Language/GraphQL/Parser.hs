{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.Parser
    ( document
    ) where

import Control.Applicative ( Alternative(..)
                           , optional
                           )
import Data.List.NonEmpty (NonEmpty(..))
import Language.GraphQL.AST
import Language.GraphQL.Lexer
import Text.Megaparsec ( lookAhead
                       , option
                       , try
                       , (<?>)
                       )

document :: Parser Document
document = spaceConsumer >> lexeme (manyNE definition)

definition :: Parser Definition
definition = DefinitionOperation <$> operationDefinition
         <|> DefinitionFragment  <$> fragmentDefinition
         <?> "definition error!"

operationDefinition :: Parser OperationDefinition
operationDefinition = OperationSelectionSet <$> selectionSet
                  <|> OperationDefinition   <$> operationType
                                            <*> optional name
                                            <*> opt variableDefinitions
                                            <*> opt directives
                                            <*> selectionSet
                  <?> "operationDefinition error"

operationType :: Parser OperationType
operationType = Query <$ symbol "query"
    <|> Mutation <$ symbol "mutation"
    <?> "operationType error"

-- * SelectionSet

selectionSet :: Parser SelectionSet
selectionSet = braces $ manyNE selection

selectionSetOpt :: Parser SelectionSetOpt
selectionSetOpt = braces $ some selection

selection :: Parser Selection
selection = SelectionField          <$> field
        <|> try (SelectionFragmentSpread <$> fragmentSpread)
        <|> SelectionInlineFragment <$> inlineFragment
        <?> "selection error!"

-- * Field

field :: Parser Field
field = Field <$> optional alias
              <*> name
              <*> opt arguments
              <*> opt directives
              <*> opt selectionSetOpt

alias :: Parser Alias
alias = try $ name <* colon

-- * Arguments

arguments :: Parser Arguments
arguments = parens $ some argument

argument :: Parser Argument
argument = Argument <$> name <* colon <*> value

-- * Fragments

fragmentSpread :: Parser FragmentSpread
fragmentSpread = FragmentSpread <$  spread
                                <*> fragmentName
                                <*> opt directives

inlineFragment :: Parser InlineFragment
inlineFragment = InlineFragment <$  spread
                                <*> optional typeCondition
                                <*> opt directives
                                <*> selectionSet

fragmentDefinition :: Parser FragmentDefinition
fragmentDefinition = FragmentDefinition
                 <$  symbol "fragment"
                 <*> name
                 <*> typeCondition
                 <*> opt directives
                 <*> selectionSet

fragmentName :: Parser FragmentName
fragmentName = but (symbol "on") *> name

typeCondition :: Parser TypeCondition
typeCondition = symbol "on" *> name

-- * Input Values

value :: Parser Value
value = ValueVariable <$> variable
    <|> ValueFloat    <$> try float
    <|> ValueInt      <$> integer
    <|> ValueBoolean  <$> booleanValue
    <|> ValueNull     <$  symbol "null"
    <|> ValueString   <$> string
    <|> ValueString   <$> blockString
    <|> ValueEnum     <$> try enumValue
    <|> ValueList     <$> listValue
    <|> ValueObject   <$> objectValue
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

variableDefinitions :: Parser VariableDefinitions
variableDefinitions = parens $ some variableDefinition

variableDefinition :: Parser VariableDefinition
variableDefinition = VariableDefinition <$> variable
                                        <*  colon
                                        <*> type_
                                        <*> optional defaultValue
variable :: Parser Variable
variable = dollar *> name

defaultValue :: Parser DefaultValue
defaultValue = equals *> value

-- * Input Types

type_ :: Parser Type
type_ = try (TypeNamed <$> name <* but "!")
    <|> TypeList       <$> brackets type_
    <|> TypeNonNull    <$> nonNullType
    <?> "type_ error!"

nonNullType :: Parser NonNullType
nonNullType = NonNullTypeNamed <$> name <* bang
          <|> NonNullTypeList  <$> brackets type_  <* bang
          <?> "nonNullType error!"

-- * Directives

directives :: Parser Directives
directives = some directive

directive :: Parser Directive
directive = Directive
        <$  at
        <*> name
        <*> opt arguments

-- * Internal

opt :: Monoid a => Parser a -> Parser a
opt = option mempty

-- Hack to reverse parser success
but :: Parser a -> Parser ()
but pn = False <$ lookAhead pn <|> pure True >>= \case
    False -> empty
    True  -> pure ()

manyNE :: Alternative f => f a -> f (NonEmpty a)
manyNE p = (:|) <$> p <*> many p
