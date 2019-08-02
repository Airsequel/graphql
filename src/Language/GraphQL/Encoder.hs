{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}

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
    = "query " <> node formatter name vars dirs sels
operationDefinition formatter (OperationDefinition Mutation name vars dirs sels)
    = "mutation " <> node formatter name vars dirs sels

node :: Formatter
     -> Maybe Name
     -> VariableDefinitions
     -> Directives
     -> SelectionSet
     -> Text
node formatter name vars dirs sels
    = fold name
    <> optempty (variableDefinitions formatter) vars
    <> optempty (directives formatter) dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

variableDefinitions :: Formatter -> [VariableDefinition] -> Text
variableDefinitions formatter
    = parensCommas formatter $ variableDefinition formatter

variableDefinition :: Formatter -> VariableDefinition -> Text
variableDefinition formatter (VariableDefinition var ty dv)
    = variable var
    <> eitherFormat formatter ": " ":"
    <> type_ ty
    <> maybe mempty (defaultValue formatter) dv

defaultValue :: Formatter -> Value -> Text
defaultValue formatter val
    = eitherFormat formatter " = " "="
    <> value formatter val

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
selection formatter (SelectionFragmentSpread x) = fragmentSpread formatter x

field :: Formatter -> Field -> Text
field formatter (Field alias name args dirs selso)
    = optempty (`Text.append` colon) (fold alias)
    <> name
    <> optempty (arguments formatter) args
    <> optempty (directives formatter) dirs
    <> selectionSetOpt'
  where
    colon = eitherFormat formatter ": " ":"
    selectionSetOpt'
        | null selso = mempty
        | otherwise = eitherFormat formatter " " mempty <> selectionSetOpt formatter selso

arguments :: Formatter -> [Argument] -> Text
arguments formatter = parensCommas formatter $ argument formatter

argument :: Formatter -> Argument -> Text
argument formatter (Argument name v)
    = name
    <> eitherFormat formatter ": " ":"
    <> value formatter v

-- * Fragments

fragmentSpread :: Formatter -> FragmentSpread -> Text
fragmentSpread formatter (FragmentSpread name ds) =
    "..." <> name <> optempty (directives formatter) ds

inlineFragment :: Formatter -> InlineFragment -> Text
inlineFragment formatter (InlineFragment tc dirs sels)
    = "... on " <> fold tc
    <> directives formatter dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

fragmentDefinition :: Formatter -> FragmentDefinition -> Text
fragmentDefinition formatter (FragmentDefinition name tc dirs sels)
    = "fragment " <> name <> " on " <> tc
    <> optempty (directives formatter) dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

-- * Values

value :: Formatter -> Value -> Text
value _ (ValueVariable x) = variable x
-- TODO: This will be replaced with `decimal` Builder
value _ (ValueInt      x) = pack $ show x
-- TODO: This will be replaced with `decimal` Builder
value _ (ValueFloat    x) = pack $ show x
value _ (ValueBoolean  x) = booleanValue x
value _ ValueNull         = mempty
value _ (ValueString   x) = stringValue x
value _ (ValueEnum     x) = x
value formatter (ValueList     x) = listValue formatter x
value formatter (ValueObject   x) = objectValue formatter x

booleanValue :: Bool -> Text
booleanValue True  = "true"
booleanValue False = "false"

-- TODO: Escape characters
stringValue :: Text -> Text
stringValue = quotes

listValue :: Formatter -> [Value] -> Text
listValue formatter = bracketsCommas formatter $ value formatter

objectValue :: Formatter -> [ObjectField] -> Text
objectValue formatter = bracesCommas $ objectField formatter

objectField :: Formatter -> ObjectField -> Text
objectField formatter (ObjectField name v) = name <> colon <> value formatter v
  where
    colon
      | Pretty _ <- formatter = ": "
      | Minified <- formatter = ":"

-- * Directives

directives :: Formatter -> [Directive] -> Text
directives formatter@(Pretty _) = Text.cons ' ' . spaces (directive formatter)
directives Minified = spaces (directive Minified)

directive :: Formatter -> Directive -> Text
directive formatter (Directive name args)
    = "@" <> name <> optempty (arguments formatter) args

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

spaces :: forall a. (a -> Text) -> [a] -> Text
spaces f = Text.intercalate "\SP" . fmap f

parensCommas :: forall a. Formatter -> (a -> Text) -> [a] -> Text
parensCommas formatter f
    = parens
    . Text.intercalate (eitherFormat formatter ", " ",")
    . fmap f

bracketsCommas :: Formatter -> (a -> Text) -> [a] -> Text
bracketsCommas formatter f
    = brackets
    . Text.intercalate (eitherFormat formatter ", " ",")
    . fmap f

bracesCommas :: forall a. (a -> Text) -> [a] -> Text
bracesCommas f = braces . Text.intercalate "," . fmap f

bracesNewLines :: forall a. (a -> Text) -> [a] -> Text
bracesNewLines f xs = Text.append (Text.intercalate "\n" $ "{" : fmap f xs) "\n}"

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs

eitherFormat :: forall a. Formatter -> a -> a -> a
eitherFormat (Pretty _) pretty _ = pretty
eitherFormat Minified _ minified = minified
