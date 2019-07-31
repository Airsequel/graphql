{-# LANGUAGE OverloadedStrings #-}
-- | This module defines a printer for the @GraphQL@ language.
module Language.GraphQL.Encoder
    ( Formatter(..)
    , definition
    , document
    ) where

import Data.Foldable (fold)
import Data.Monoid ((<>))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Language.GraphQL.AST

-- | Instructs the encoder whether a GraphQL should be minified or pretty
--   printed.
data Formatter
    = Minified
    | Pretty Int

-- | Converts a 'Document' into a string.
document :: Formatter -> Document -> Text
document formatter defs
    | Pretty _ <- formatter = Text.intercalate "\n" encodeDocument
    | Minified <-formatter = Text.snoc (mconcat encodeDocument) '\n'
  where
    encodeDocument = NonEmpty.toList $ definition formatter <$> defs

-- | Converts a 'Definition' into a string.
definition :: Formatter -> Definition -> Text
definition formatter x
    | Pretty _ <- formatter = Text.snoc (encodeDefinition x) '\n'
    | Minified <- formatter = encodeDefinition x
  where
    encodeDefinition (DefinitionOperation operation)
        = operationDefinition formatter operation
    encodeDefinition (DefinitionFragment fragment)
        = fragmentDefinition formatter fragment

operationDefinition :: Formatter -> OperationDefinition -> Text
operationDefinition formatter (OperationSelectionSet sels)
    = selectionSet formatter sels
operationDefinition formatter (OperationDefinition Query name vars dirs sels)
    = "query " <> node formatter (fold name) vars dirs sels
operationDefinition formatter (OperationDefinition Mutation name vars dirs sels)
    = "mutation " <> node formatter (fold name) vars dirs sels

node :: Formatter
     -> Name
     -> VariableDefinitions
     -> Directives
     -> SelectionSet
     -> Text
node formatter name vars dirs sels
    = name
    <> optempty variableDefinitions vars
    <> optempty directives dirs
    <> selectionSet formatter sels

variableDefinitions :: [VariableDefinition] -> Text
variableDefinitions = parensCommas variableDefinition

variableDefinition :: VariableDefinition -> Text
variableDefinition (VariableDefinition var ty dv) =
    variable var <> ":" <> type_ ty <> maybe mempty defaultValue dv

defaultValue :: Value -> Text
defaultValue val = "=" <> value val

variable :: Name -> Text
variable var = "$" <> var

selectionSet :: Formatter -> SelectionSet -> Text
selectionSet formatter@(Pretty _) = bracesNewLines (selection formatter) . NonEmpty.toList
selectionSet Minified = bracesCommas (selection Minified) . NonEmpty.toList

selectionSetOpt :: Formatter -> SelectionSetOpt -> Text
selectionSetOpt formatter@(Pretty _) = bracesNewLines $ selection formatter
selectionSetOpt Minified = bracesCommas $ selection Minified

selection :: Formatter -> Selection -> Text
selection formatter (SelectionField          x) = field formatter x
selection formatter (SelectionInlineFragment x) = inlineFragment formatter x
selection _ (SelectionFragmentSpread x) = fragmentSpread x

field :: Formatter -> Field -> Text
field formatter (Field alias name args dirs selso) =
       optempty (`Text.append` ":") (fold alias)
    <> name
    <> optempty arguments args
    <> optempty directives dirs
    <> optempty (selectionSetOpt formatter) selso

arguments :: [Argument] -> Text
arguments = parensCommas argument

argument :: Argument -> Text
argument (Argument name v) = name <> ":" <> value v

-- * Fragments

fragmentSpread :: FragmentSpread -> Text
fragmentSpread (FragmentSpread name ds) =
    "..." <> name <> optempty directives ds

inlineFragment :: Formatter -> InlineFragment -> Text
inlineFragment formatter (InlineFragment tc dirs sels) =
    "... on " <> fold tc
              <> directives dirs
              <> selectionSet formatter sels

fragmentDefinition :: Formatter -> FragmentDefinition -> Text
fragmentDefinition formatter (FragmentDefinition name tc dirs sels) =
    "fragment " <> name <> " on " <> tc
                <> optempty directives dirs
                <> selectionSet formatter sels

-- * Values

value :: Value -> Text
value (ValueVariable x) = variable x
-- TODO: This will be replaced with `decimal` Builder
value (ValueInt      x) = pack $ show x
-- TODO: This will be replaced with `decimal` Builder
value (ValueFloat    x) = pack $ show x
value (ValueBoolean  x) = booleanValue x
value ValueNull         = mempty
value (ValueString   x) = stringValue x
value (ValueEnum     x) = x
value (ValueList     x) = listValue x
value (ValueObject   x) = objectValue x

booleanValue :: Bool -> Text
booleanValue True  = "true"
booleanValue False = "false"

-- TODO: Escape characters
stringValue :: Text -> Text
stringValue = quotes

listValue :: [Value] -> Text
listValue = bracketsCommas value

objectValue :: [ObjectField] -> Text
objectValue = bracesCommas objectField

objectField :: ObjectField -> Text
objectField (ObjectField name v) = name <> ":" <> value v

-- * Directives

directives :: [Directive] -> Text
directives = spaces directive

directive :: Directive -> Text
directive (Directive name args) = "@" <> name <> optempty arguments args

-- * Type Reference

type_ :: Type -> Text
type_ (TypeNamed   x) = x
type_ (TypeList    x) = listType x
type_ (TypeNonNull x) = nonNullType x

listType :: Type -> Text
listType x = brackets (type_ x)

nonNullType :: NonNullType -> Text
nonNullType (NonNullTypeNamed x) = x <> "!"
nonNullType (NonNullTypeList  x) = listType x <> "!"

-- * Internal

between :: Char -> Char -> Text -> Text
between open close = Text.cons open . (`Text.snoc` close)

parens :: Text -> Text
parens = between '(' ')'

brackets :: Text -> Text
brackets = between '[' ']'

braces :: Text -> Text
braces = between '{' '}'

quotes :: Text -> Text
quotes = between '"' '"'

spaces :: (a -> Text) -> [a] -> Text
spaces f = Text.intercalate "\SP" . fmap f

parensCommas :: (a -> Text) -> [a] -> Text
parensCommas f = parens . Text.intercalate "," . fmap f

bracketsCommas :: (a -> Text) -> [a] -> Text
bracketsCommas f = brackets . Text.intercalate "," . fmap f

bracesCommas :: (a -> Text) -> [a] -> Text
bracesCommas f = braces . Text.intercalate "," . fmap f

bracesNewLines :: (a -> Text) -> [a] -> Text
bracesNewLines f xs = Text.append (Text.intercalate "\n" $ "{" : fmap f xs) "\n}"

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs
