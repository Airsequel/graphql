{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}

-- | This module defines a minifier and a printer for the @GraphQL@ language.
module Language.GraphQL.AST.Encoder
    ( Formatter
    , definition
    , directive
    , document
    , minified
    , pretty
    , type'
    , value
    ) where

import Data.Foldable (fold)
import Data.Monoid ((<>))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import qualified Language.GraphQL.AST as Full

-- | Instructs the encoder whether the GraphQL document should be minified or
--   pretty printed.
--
--   Use 'pretty' or 'minified' to construct the formatter.
data Formatter
    = Minified
    | Pretty Word

-- | Constructs a formatter for pretty printing.
pretty :: Formatter
pretty = Pretty 0

-- | Constructs a formatter for minifying.
minified :: Formatter
minified = Minified

-- | Converts a 'Full.Document' into a string.
document :: Formatter -> Full.Document -> Text
document formatter defs
    | Pretty _ <- formatter = Text.Lazy.intercalate "\n" encodeDocument
    | Minified <-formatter = Text.Lazy.snoc (mconcat encodeDocument) '\n'
  where
    encodeDocument = NonEmpty.toList $ definition formatter <$> defs

-- | Converts a 'Full.Definition' into a string.
definition :: Formatter -> Full.Definition -> Text
definition formatter x
    | Pretty _ <- formatter = Text.Lazy.snoc (encodeDefinition x) '\n'
    | Minified <- formatter = encodeDefinition x
  where
    encodeDefinition (Full.DefinitionOperation operation)
        = operationDefinition formatter operation
    encodeDefinition (Full.DefinitionFragment fragment)
        = fragmentDefinition formatter fragment

operationDefinition :: Formatter -> Full.OperationDefinition -> Text
operationDefinition formatter (Full.OperationSelectionSet sels)
    = selectionSet formatter sels
operationDefinition formatter (Full.OperationDefinition Full.Query name vars dirs sels)
    = "query " <> node formatter name vars dirs sels
operationDefinition formatter (Full.OperationDefinition Full.Mutation name vars dirs sels)
    = "mutation " <> node formatter name vars dirs sels

node :: Formatter
    -> Maybe Full.Name
    -> [Full.VariableDefinition]
    -> [Full.Directive]
    -> Full.SelectionSet
    -> Text
node formatter name vars dirs sels
    = Text.Lazy.fromStrict (fold name)
    <> optempty (variableDefinitions formatter) vars
    <> optempty (directives formatter) dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

variableDefinitions :: Formatter -> [Full.VariableDefinition] -> Text
variableDefinitions formatter
    = parensCommas formatter $ variableDefinition formatter

variableDefinition :: Formatter -> Full.VariableDefinition -> Text
variableDefinition formatter (Full.VariableDefinition var ty dv)
    = variable var
    <> eitherFormat formatter ": " ":"
    <> type' ty
    <> maybe mempty (defaultValue formatter) dv

defaultValue :: Formatter -> Full.Value -> Text
defaultValue formatter val
    = eitherFormat formatter " = " "="
    <> value formatter val

variable :: Full.Name -> Text
variable var = "$" <> Text.Lazy.fromStrict var

selectionSet :: Formatter -> Full.SelectionSet -> Text
selectionSet formatter
    = bracesList formatter (selection formatter)
    . NonEmpty.toList

selectionSetOpt :: Formatter -> Full.SelectionSetOpt -> Text
selectionSetOpt formatter = bracesList formatter $ selection formatter

selection :: Formatter -> Full.Selection -> Text
selection formatter = Text.Lazy.append indent . f
  where
    f (Full.SelectionField x) = field incrementIndent x
    f (Full.SelectionInlineFragment x) = inlineFragment incrementIndent x
    f (Full.SelectionFragmentSpread x) = fragmentSpread incrementIndent x
    incrementIndent
        | Pretty n <- formatter = Pretty $ n + 1
        | otherwise = Minified
    indent
        | Pretty n <- formatter = Text.Lazy.replicate (fromIntegral $ n + 1) "  "
        | otherwise = mempty

field :: Formatter -> Full.Field -> Text
field formatter (Full.Field alias name args dirs selso)
    = optempty (`Text.Lazy.append` colon) (Text.Lazy.fromStrict $ fold alias)
    <> Text.Lazy.fromStrict name
    <> optempty (arguments formatter) args
    <> optempty (directives formatter) dirs
    <> selectionSetOpt'
  where
    colon = eitherFormat formatter ": " ":"
    selectionSetOpt'
        | null selso = mempty
        | otherwise = eitherFormat formatter " " mempty <> selectionSetOpt formatter selso

arguments :: Formatter -> [Full.Argument] -> Text
arguments formatter = parensCommas formatter $ argument formatter

argument :: Formatter -> Full.Argument -> Text
argument formatter (Full.Argument name v)
    = Text.Lazy.fromStrict name
    <> eitherFormat formatter ": " ":"
    <> value formatter v

-- * Fragments

fragmentSpread :: Formatter -> Full.FragmentSpread -> Text
fragmentSpread formatter (Full.FragmentSpread name ds)
    = "..." <> Text.Lazy.fromStrict name <> optempty (directives formatter) ds

inlineFragment :: Formatter -> Full.InlineFragment -> Text
inlineFragment formatter (Full.InlineFragment tc dirs sels)
    = "... on "
    <> Text.Lazy.fromStrict (fold tc)
    <> directives formatter dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

fragmentDefinition :: Formatter -> Full.FragmentDefinition -> Text
fragmentDefinition formatter (Full.FragmentDefinition name tc dirs sels)
    = "fragment " <> Text.Lazy.fromStrict name
    <> " on " <> Text.Lazy.fromStrict tc
    <> optempty (directives formatter) dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

-- * Miscellaneous

-- | Converts a 'Full.Directive' into a string.
directive :: Formatter -> Full.Directive -> Text
directive formatter (Full.Directive name args)
    = "@" <> Text.Lazy.fromStrict name <> optempty (arguments formatter) args

directives :: Formatter -> [Full.Directive] -> Text
directives formatter@(Pretty _) = Text.Lazy.cons ' ' . spaces (directive formatter)
directives Minified = spaces (directive Minified)

-- | Converts a 'Full.Value' into a string.
value :: Formatter -> Full.Value -> Text
value _ (Full.Variable x) = variable x
value _ (Full.Int x) = toLazyText $ decimal x
value _ (Full.Float x) = toLazyText $ realFloat x
value _ (Full.Boolean  x) = booleanValue x
value _ Full.Null = mempty
value _ (Full.String x) = stringValue $ Text.Lazy.fromStrict x
value _ (Full.Enum x) = Text.Lazy.fromStrict x
value formatter (Full.List x) = listValue formatter x
value formatter (Full.Object x) = objectValue formatter x

booleanValue :: Bool -> Text
booleanValue True  = "true"
booleanValue False = "false"

stringValue :: Text -> Text
stringValue
    = quotes
    . Text.Lazy.replace "\"" "\\\""
    . Text.Lazy.replace "\\" "\\\\"

listValue :: Formatter -> [Full.Value] -> Text
listValue formatter = bracketsCommas formatter $ value formatter

objectValue :: Formatter -> [Full.ObjectField] -> Text
objectValue formatter = intercalate $ objectField formatter
  where
    intercalate f
        = braces
        . Text.Lazy.intercalate (eitherFormat formatter ", " ",")
        . fmap f


objectField :: Formatter -> Full.ObjectField -> Text
objectField formatter (Full.ObjectField name v)
    = Text.Lazy.fromStrict name <> colon <> value formatter v
  where
    colon
      | Pretty _ <- formatter = ": "
      | Minified <- formatter = ":"

-- | Converts a 'Full.Type' a type into a string.
type' :: Full.Type -> Text
type' (Full.TypeNamed   x) = Text.Lazy.fromStrict x
type' (Full.TypeList    x) = listType x
type' (Full.TypeNonNull x) = nonNullType x

listType :: Full.Type -> Text
listType x = brackets (type' x)

nonNullType :: Full.NonNullType -> Text
nonNullType (Full.NonNullTypeNamed x) = Text.Lazy.fromStrict x <> "!"
nonNullType (Full.NonNullTypeList  x) = listType x <> "!"

-- * Internal

between :: Char -> Char -> Text -> Text
between open close = Text.Lazy.cons open . (`Text.Lazy.snoc` close)

parens :: Text -> Text
parens = between '(' ')'

brackets :: Text -> Text
brackets = between '[' ']'

braces :: Text -> Text
braces = between '{' '}'

quotes :: Text -> Text
quotes = between '"' '"'

spaces :: forall a. (a -> Text) -> [a] -> Text
spaces f = Text.Lazy.intercalate "\SP" . fmap f

parensCommas :: forall a. Formatter -> (a -> Text) -> [a] -> Text
parensCommas formatter f
    = parens
    . Text.Lazy.intercalate (eitherFormat formatter ", " ",")
    . fmap f

bracketsCommas :: Formatter -> (a -> Text) -> [a] -> Text
bracketsCommas formatter f
    = brackets
    . Text.Lazy.intercalate (eitherFormat formatter ", " ",")
    . fmap f

bracesList :: forall a. Formatter -> (a -> Text) -> [a] -> Text
bracesList (Pretty intendation) f xs
    = Text.Lazy.snoc (Text.Lazy.intercalate "\n" content) '\n'
    <> (Text.Lazy.snoc $ Text.Lazy.replicate (fromIntegral intendation) "  ") '}'
  where
    content = "{" : fmap f xs
bracesList Minified f xs = braces $ Text.Lazy.intercalate "," $ fmap f xs

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs

eitherFormat :: forall a. Formatter -> a -> a -> a
eitherFormat (Pretty _) x _ = x
eitherFormat Minified _ x = x
