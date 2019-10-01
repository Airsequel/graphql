{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}

-- | This module defines a minifier and a printer for the @GraphQL@ language.
module Language.GraphQL.Encoder
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
import Language.GraphQL.AST

-- | Instructs the encoder whether a GraphQL should be minified or pretty
--   printed.
--   
--   Use 'pretty' and 'minified' to construct the formatter.
data Formatter
    = Minified
    | Pretty Word

-- | Constructs a formatter for pretty printing.
pretty :: Formatter
pretty = Pretty 0

-- | Constructs a formatter for minifying.
minified :: Formatter
minified = Minified

-- | Converts a 'Document' into a string.
document :: Formatter -> Document -> Text
document formatter defs
    | Pretty _ <- formatter = Text.Lazy.intercalate "\n" encodeDocument
    | Minified <-formatter = Text.Lazy.snoc (mconcat encodeDocument) '\n'
  where
    encodeDocument = NonEmpty.toList $ definition formatter <$> defs

-- | Converts a 'Definition' into a string.
definition :: Formatter -> Definition -> Text
definition formatter x
    | Pretty _ <- formatter = Text.Lazy.snoc (encodeDefinition x) '\n'
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
    -> [VariableDefinition]
    -> [Directive]
    -> SelectionSet
    -> Text
node formatter name vars dirs sels
    = Text.Lazy.fromStrict (fold name)
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
    <> type' ty
    <> maybe mempty (defaultValue formatter) dv

defaultValue :: Formatter -> Value -> Text
defaultValue formatter val
    = eitherFormat formatter " = " "="
    <> value formatter val

variable :: Name -> Text
variable var = "$" <> Text.Lazy.fromStrict var

selectionSet :: Formatter -> SelectionSet -> Text
selectionSet formatter
    = bracesList formatter (selection formatter)
    . NonEmpty.toList

selectionSetOpt :: Formatter -> SelectionSetOpt -> Text
selectionSetOpt formatter = bracesList formatter $ selection formatter

selection :: Formatter -> Selection -> Text
selection formatter = Text.Lazy.append indent . f
  where
    f (SelectionField x) = field incrementIndent x
    f (SelectionInlineFragment x) = inlineFragment incrementIndent x
    f (SelectionFragmentSpread x) = fragmentSpread incrementIndent x
    incrementIndent
        | Pretty n <- formatter = Pretty $ n + 1
        | otherwise = Minified
    indent
        | Pretty n <- formatter = Text.Lazy.replicate (fromIntegral $ n + 1) "  "
        | otherwise = mempty

field :: Formatter -> Field -> Text
field formatter (Field alias name args dirs selso)
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

arguments :: Formatter -> [Argument] -> Text
arguments formatter = parensCommas formatter $ argument formatter

argument :: Formatter -> Argument -> Text
argument formatter (Argument name v)
    = Text.Lazy.fromStrict name
    <> eitherFormat formatter ": " ":"
    <> value formatter v

-- * Fragments

fragmentSpread :: Formatter -> FragmentSpread -> Text
fragmentSpread formatter (FragmentSpread name ds)
    = "..." <> Text.Lazy.fromStrict name <> optempty (directives formatter) ds

inlineFragment :: Formatter -> InlineFragment -> Text
inlineFragment formatter (InlineFragment tc dirs sels)
    = "... on "
    <> Text.Lazy.fromStrict (fold tc)
    <> directives formatter dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

fragmentDefinition :: Formatter -> FragmentDefinition -> Text
fragmentDefinition formatter (FragmentDefinition name tc dirs sels)
    = "fragment " <> Text.Lazy.fromStrict name
    <> " on " <> Text.Lazy.fromStrict tc
    <> optempty (directives formatter) dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

-- * Miscellaneous

-- | Converts a 'Directive' into a string.
directive :: Formatter -> Directive -> Text
directive formatter (Directive name args)
    = "@" <> Text.Lazy.fromStrict name <> optempty (arguments formatter) args

directives :: Formatter -> [Directive] -> Text
directives formatter@(Pretty _) = Text.Lazy.cons ' ' . spaces (directive formatter)
directives Minified = spaces (directive Minified)

-- | Converts a 'Value' into a string.
value :: Formatter -> Value -> Text
value _ (ValueVariable x) = variable x
value _ (ValueInt x) = toLazyText $ decimal x
value _ (ValueFloat x) = toLazyText $ realFloat x
value _ (ValueBoolean  x) = booleanValue x
value _ ValueNull = mempty
value _ (ValueString x) = stringValue $ Text.Lazy.fromStrict x
value _ (ValueEnum x) = Text.Lazy.fromStrict x
value formatter (ValueList x) = listValue formatter x
value formatter (ValueObject x) = objectValue formatter x

booleanValue :: Bool -> Text
booleanValue True  = "true"
booleanValue False = "false"

stringValue :: Text -> Text
stringValue
    = quotes
    . Text.Lazy.replace "\"" "\\\""
    . Text.Lazy.replace "\\" "\\\\"

listValue :: Formatter -> [Value] -> Text
listValue formatter = bracketsCommas formatter $ value formatter

objectValue :: Formatter -> [ObjectField] -> Text
objectValue formatter = intercalate $ objectField formatter
  where
    intercalate f
        = braces
        . Text.Lazy.intercalate (eitherFormat formatter ", " ",")
        . fmap f


objectField :: Formatter -> ObjectField -> Text
objectField formatter (ObjectField name v)
    = Text.Lazy.fromStrict name <> colon <> value formatter v
  where
    colon
      | Pretty _ <- formatter = ": "
      | Minified <- formatter = ":"

-- | Converts a 'Type' a type into a string.
type' :: Type -> Text
type' (TypeNamed   x) = Text.Lazy.fromStrict x
type' (TypeList    x) = listType x
type' (TypeNonNull x) = nonNullType x

listType :: Type -> Text
listType x = brackets (type' x)

nonNullType :: NonNullType -> Text
nonNullType (NonNullTypeNamed x) = Text.Lazy.fromStrict x <> "!"
nonNullType (NonNullTypeList  x) = listType x <> "!"

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
