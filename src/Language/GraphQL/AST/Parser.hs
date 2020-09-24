{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | @GraphQL@ document parser.
module Language.GraphQL.AST.Parser
    ( document
    ) where

import Control.Applicative (Alternative(..), liftA2, optional)
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
import Text.Megaparsec
    ( MonadParsec(..)
    , SourcePos(..)
    , getSourcePos
    , lookAhead
    , option
    , try
    , unPos
    , (<?>)
    )

-- | Parser for the GraphQL documents.
document :: Parser Document
document = unicodeBOM
    *> spaceConsumer
    *> lexeme (NonEmpty.some definition)

definition :: Parser Definition
definition = ExecutableDefinition <$> executableDefinition
    <|> typeSystemDefinition'
    <|> typeSystemExtension'
    <?> "Definition"
  where
    typeSystemDefinition' = do
        location <- getLocation
        definition' <- typeSystemDefinition
        pure $ TypeSystemDefinition definition' location
    typeSystemExtension' = do
        location <- getLocation
        definition' <- typeSystemExtension
        pure $ TypeSystemExtension definition' location

getLocation :: Parser Location
getLocation = fromSourcePosition <$> getSourcePos
  where
    fromSourcePosition SourcePos{..} =
        Location (wordFromPosition sourceLine) (wordFromPosition sourceColumn)
    wordFromPosition = fromIntegral . unPos

executableDefinition :: Parser ExecutableDefinition
executableDefinition = DefinitionOperation <$> operationDefinition
    <|> DefinitionFragment  <$> fragmentDefinition
    <?> "ExecutableDefinition"

typeSystemDefinition :: Parser TypeSystemDefinition
typeSystemDefinition = schemaDefinition
    <|> typeSystemDefinitionWithDescription
    <?> "TypeSystemDefinition"
  where
    typeSystemDefinitionWithDescription = description
        >>= liftA2 (<|>) typeDefinition' directiveDefinition
    typeDefinition' description' = TypeDefinition
        <$> typeDefinition description'

typeSystemExtension :: Parser TypeSystemExtension
typeSystemExtension = SchemaExtension <$> schemaExtension
    <|> TypeExtension <$> typeExtension
    <?> "TypeSystemExtension"

directiveDefinition :: Description -> Parser TypeSystemDefinition
directiveDefinition description' = DirectiveDefinition description'
    <$ symbol "directive"
    <* at
    <*> name
    <*> argumentsDefinition
    <* symbol "on"
    <*> directiveLocations
    <?> "DirectiveDefinition"

directiveLocations :: Parser (NonEmpty DirectiveLocation)
directiveLocations = optional pipe
    *> directiveLocation `NonEmpty.sepBy1` pipe
    <?> "DirectiveLocations"

directiveLocation :: Parser DirectiveLocation
directiveLocation
    = Directive.ExecutableDirectiveLocation <$> executableDirectiveLocation
    <|> Directive.TypeSystemDirectiveLocation <$> typeSystemDirectiveLocation
    <?> "DirectiveLocation"

executableDirectiveLocation :: Parser ExecutableDirectiveLocation
executableDirectiveLocation = Directive.Query <$ symbol "QUERY"
    <|> Directive.Mutation <$ symbol "MUTATION"
    <|> Directive.Subscription <$ symbol "SUBSCRIPTION"
    <|> Directive.Field <$ symbol "FIELD"
    <|> Directive.FragmentDefinition <$ "FRAGMENT_DEFINITION"
    <|> Directive.FragmentSpread <$ "FRAGMENT_SPREAD"
    <|> Directive.InlineFragment <$ "INLINE_FRAGMENT"
    <?> "ExecutableDirectiveLocation"

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
    <?> "TypeSystemDirectiveLocation"

typeDefinition :: Description -> Parser TypeDefinition
typeDefinition description' = scalarTypeDefinition description'
    <|> objectTypeDefinition description'
    <|> interfaceTypeDefinition description'
    <|> unionTypeDefinition description'
    <|> enumTypeDefinition description'
    <|> inputObjectTypeDefinition description'
    <?> "TypeDefinition"

typeExtension :: Parser TypeExtension
typeExtension = scalarTypeExtension
    <|> objectTypeExtension
    <|> interfaceTypeExtension
    <|> unionTypeExtension
    <|> enumTypeExtension
    <|> inputObjectTypeExtension
    <?> "TypeExtension"

scalarTypeDefinition :: Description -> Parser TypeDefinition
scalarTypeDefinition description' = ScalarTypeDefinition description'
    <$ symbol "scalar"
    <*> name
    <*> directives
    <?> "ScalarTypeDefinition"

scalarTypeExtension :: Parser TypeExtension
scalarTypeExtension = extend "scalar" "ScalarTypeExtension"
    $ (ScalarTypeExtension <$> name <*> NonEmpty.some directive) :| []

objectTypeDefinition :: Description -> Parser TypeDefinition
objectTypeDefinition description' = ObjectTypeDefinition description'
    <$ symbol "type"
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
    <$> optional stringValue
    <?> "Description"

unionTypeDefinition :: Description -> Parser TypeDefinition
unionTypeDefinition description' = UnionTypeDefinition description'
    <$ symbol "union"
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

interfaceTypeDefinition :: Description -> Parser TypeDefinition
interfaceTypeDefinition description' = InterfaceTypeDefinition description'
    <$ symbol "interface"
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

enumTypeDefinition :: Description -> Parser TypeDefinition
enumTypeDefinition description' = EnumTypeDefinition description'
    <$ symbol "enum"
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

inputObjectTypeDefinition :: Description -> Parser TypeDefinition
inputObjectTypeDefinition description' = InputObjectTypeDefinition description'
    <$ symbol "input"
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
operationDefinition = shorthand
    <|> operationDefinition'
    <?> "OperationDefinition"
  where
    shorthand = do
        location <- getLocation
        selectionSet' <- selectionSet
        pure $ SelectionSet selectionSet' location
    operationDefinition' = do
        location <- getLocation
        operationType' <- operationType
        operationName <- optional name
        variableDefinitions' <- variableDefinitions
        directives' <- directives
        selectionSet' <- selectionSet
        pure $ OperationDefinition operationType' operationName variableDefinitions' directives' selectionSet' location

operationType :: Parser OperationType
operationType = Query <$ symbol "query"
    <|> Mutation <$ symbol "mutation"
    <|> Subscription <$ symbol "subscription"
    <?> "OperationType"

selectionSet :: Parser SelectionSet
selectionSet = braces (NonEmpty.some selection) <?> "SelectionSet"

selectionSetOpt :: Parser SelectionSetOpt
selectionSetOpt = listOptIn braces selection <?> "SelectionSet"

selection :: Parser Selection
selection = FieldSelection <$> field
    <|> FragmentSpreadSelection <$> try fragmentSpread
    <|> InlineFragmentSelection <$> inlineFragment
    <?> "Selection"

field :: Parser Field
field = label "Field" $ do
    location <- getLocation
    alias' <- optional alias
    name' <- name
    arguments' <- arguments
    directives' <- directives
    selectionSetOpt' <- selectionSetOpt
    pure $ Field alias' name' arguments' directives' selectionSetOpt' location

alias :: Parser Name
alias = try (name <* colon) <?> "Alias"

arguments :: Parser [Argument]
arguments = listOptIn parens argument <?> "Arguments"

argument :: Parser Argument
argument = label "Argument" $ do
    location <- getLocation
    name' <- name
    colon
    value' <- valueNode
    pure $ Argument name' value' location

fragmentSpread :: Parser FragmentSpread
fragmentSpread = label "FragmentSpread" $ do
    location <- getLocation
    _ <- spread
    fragmentName' <- fragmentName
    directives' <- directives
    pure $ FragmentSpread fragmentName' directives' location

inlineFragment :: Parser InlineFragment
inlineFragment = label "InlineFragment" $ do
    location <- getLocation
    _ <- spread
    typeCondition' <- optional typeCondition
    directives' <- directives
    selectionSet' <- selectionSet
    pure $ InlineFragment typeCondition' directives' selectionSet' location

fragmentDefinition :: Parser FragmentDefinition
fragmentDefinition =  label "FragmentDefinition" $ do
    location <- getLocation
    _ <- symbol "fragment"
    fragmentName' <- name
    typeCondition' <- typeCondition
    directives' <- directives
    selectionSet' <- selectionSet
    pure $ FragmentDefinition
        fragmentName' typeCondition' directives' selectionSet' location

fragmentName :: Parser Name
fragmentName = but (symbol "on") *> name <?> "FragmentName"

typeCondition :: Parser TypeCondition
typeCondition = symbol "on" *> name <?> "TypeCondition"

valueNode :: Parser (Node Value)
valueNode = do
    location <- getLocation
    value' <- value
    pure $ Node value' location

value :: Parser Value
value = Variable <$> variable
    <|> Float <$> try float
    <|> Int <$> integer
    <|> Boolean <$> booleanValue
    <|> Null <$  nullValue
    <|> String <$> stringValue
    <|> Enum <$> try enumValue
    <|> List <$> brackets (some value)
    <|> Object <$> braces (some $ objectField value)
    <?> "Value"

constValue :: Parser ConstValue
constValue = ConstFloat <$> try float
    <|> ConstInt <$> integer
    <|> ConstBoolean <$> booleanValue
    <|> ConstNull <$ nullValue
    <|> ConstString <$> stringValue
    <|> ConstEnum <$> try enumValue
    <|> ConstList <$> brackets (some constValue)
    <|> ConstObject <$> braces (some $ objectField constValue)
    <?> "Value"

booleanValue :: Parser Bool
booleanValue = True  <$ symbol "true"
    <|> False <$ symbol "false"
    <?> "BooleanValue"

enumValue :: Parser Name
enumValue = but (symbol "true")
    *> but (symbol "false")
    *> but (symbol "null")
    *> name
    <?> "EnumValue"

stringValue :: Parser Text
stringValue = blockString <|> string <?> "StringValue"

nullValue :: Parser Text
nullValue = symbol "null" <?> "NullValue"

objectField :: Parser a -> Parser (ObjectField a)
objectField valueParser = label "ObjectField" $ do
    location <- getLocation
    fieldName <- name
    colon
    fieldValue <- valueParser
    pure $ ObjectField fieldName fieldValue location

variableDefinitions :: Parser [VariableDefinition]
variableDefinitions = listOptIn parens variableDefinition
    <?> "VariableDefinitions"

variableDefinition :: Parser VariableDefinition
variableDefinition = label "VariableDefinition" $ do
    location <- getLocation
    variableName <- variable
    colon
    variableType <- type'
    variableValue <- defaultValue
    pure $ VariableDefinition variableName variableType variableValue location

variable :: Parser Name
variable = dollar *> name <?> "Variable"

defaultValue :: Parser (Maybe ConstValue)
defaultValue = optional (equals *> constValue) <?> "DefaultValue"

type' :: Parser Type
type' = try (TypeNonNull <$> nonNullType)
    <|> TypeList <$> brackets type'
    <|> TypeNamed <$> name
    <?> "Type"

nonNullType :: Parser NonNullType
nonNullType = NonNullTypeNamed <$> name <* bang
    <|> NonNullTypeList  <$> brackets type'  <* bang
    <?> "NonNullType"

directives :: Parser [Directive]
directives = many directive <?> "Directives"

directive :: Parser Directive
directive = label "Directive" $ do
    location <- getLocation
    at
    directiveName <- name
    directiveArguments <- arguments
    pure $ Directive directiveName directiveArguments location

listOptIn :: (Parser [a] -> Parser [a]) -> Parser a -> Parser [a]
listOptIn surround = option [] . surround . some

-- Hack to reverse parser success
but :: Parser a -> Parser ()
but pn = False <$ lookAhead pn <|> pure True >>= \case
    False -> empty
    True  -> pure ()
