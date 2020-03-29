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

import Data.Char (ord)
import Data.Foldable (fold)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import qualified Language.GraphQL.AST as Full
import Language.GraphQL.AST.Document

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

-- | Converts a Document' into a string.
document :: Formatter -> Document -> Lazy.Text
document formatter defs
    | Pretty _ <- formatter = Lazy.Text.intercalate "\n" encodeDocument
    | Minified <-formatter = Lazy.Text.snoc (mconcat encodeDocument) '\n'
  where
    encodeDocument = foldr executableDefinition [] defs
    executableDefinition (ExecutableDefinition x) acc = definition formatter x : acc
    executableDefinition _ acc = acc

-- | Converts a t'Full.ExecutableDefinition' into a string.
definition :: Formatter -> ExecutableDefinition -> Lazy.Text
definition formatter x
    | Pretty _ <- formatter = Lazy.Text.snoc (encodeDefinition x) '\n'
    | Minified <- formatter = encodeDefinition x
  where
    encodeDefinition (Full.DefinitionOperation operation)
        = operationDefinition formatter operation
    encodeDefinition (Full.DefinitionFragment fragment)
        = fragmentDefinition formatter fragment

-- | Converts a 'Full.OperationDefinition into a string.
operationDefinition :: Formatter -> Full.OperationDefinition -> Lazy.Text
operationDefinition formatter (Full.SelectionSet sels)
    = selectionSet formatter sels
operationDefinition formatter (Full.OperationDefinition Full.Query name vars dirs sels)
    = "query " <> node formatter name vars dirs sels
operationDefinition formatter (Full.OperationDefinition Full.Mutation name vars dirs sels)
    = "mutation " <> node formatter name vars dirs sels

-- | Converts a Full.Query or Full.Mutation into a string.
node :: Formatter ->
    Maybe Full.Name ->
    [Full.VariableDefinition] ->
    [Full.Directive] ->
    Full.SelectionSet ->
    Lazy.Text
node formatter name vars dirs sels
    = Lazy.Text.fromStrict (fold name)
    <> optempty (variableDefinitions formatter) vars
    <> optempty (directives formatter) dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

variableDefinitions :: Formatter -> [Full.VariableDefinition] -> Lazy.Text
variableDefinitions formatter
    = parensCommas formatter $ variableDefinition formatter

variableDefinition :: Formatter -> Full.VariableDefinition -> Lazy.Text
variableDefinition formatter (Full.VariableDefinition var ty dv)
    = variable var
    <> eitherFormat formatter ": " ":"
    <> type' ty
    <> maybe mempty (defaultValue formatter) dv

defaultValue :: Formatter -> Full.Value -> Lazy.Text
defaultValue formatter val
    = eitherFormat formatter " = " "="
    <> value formatter val

variable :: Full.Name -> Lazy.Text
variable var = "$" <> Lazy.Text.fromStrict var

selectionSet :: Formatter -> Full.SelectionSet -> Lazy.Text
selectionSet formatter
    = bracesList formatter (selection formatter)
    . NonEmpty.toList

selectionSetOpt :: Formatter -> Full.SelectionSetOpt -> Lazy.Text
selectionSetOpt formatter = bracesList formatter $ selection formatter

indentSymbol :: Lazy.Text
indentSymbol = "  "

indent :: (Integral a) => a -> Lazy.Text
indent indentation = Lazy.Text.replicate (fromIntegral indentation) indentSymbol

selection :: Formatter -> Full.Selection -> Lazy.Text
selection formatter = Lazy.Text.append indent' . encodeSelection
  where
    encodeSelection (Full.Field alias name args directives' selections) =
        field incrementIndent alias name args directives' selections
    encodeSelection (Full.InlineFragment typeCondition directives' selections) =
        inlineFragment incrementIndent typeCondition directives' selections
    encodeSelection (Full.FragmentSpread name directives') =
        fragmentSpread incrementIndent name directives'
    incrementIndent
        | Pretty indentation <- formatter = Pretty $ indentation + 1
        | otherwise = Minified
    indent'
        | Pretty indentation <- formatter = indent $ indentation + 1
        | otherwise = ""

colon :: Formatter -> Lazy.Text
colon formatter = eitherFormat formatter ": " ":"

-- | Converts Full.Field into a string
field :: Formatter ->
    Maybe Full.Name ->
    Full.Name ->
    [Full.Argument] ->
    [Full.Directive] ->
    [Full.Selection] ->
    Lazy.Text
field formatter alias name args dirs set
    = optempty prependAlias (fold alias)
    <> Lazy.Text.fromStrict name
    <> optempty (arguments formatter) args
    <> optempty (directives formatter) dirs
    <> optempty selectionSetOpt' set
  where
    prependAlias aliasName = Lazy.Text.fromStrict aliasName <>  colon formatter
    selectionSetOpt' = (eitherFormat formatter " " "" <>)
        . selectionSetOpt formatter

arguments :: Formatter -> [Full.Argument] -> Lazy.Text
arguments formatter = parensCommas formatter $ argument formatter

argument :: Formatter -> Full.Argument -> Lazy.Text
argument formatter (Full.Argument name value')
    = Lazy.Text.fromStrict name
    <> colon formatter
    <> value formatter value'

-- * Fragments

fragmentSpread :: Formatter -> Full.Name -> [Full.Directive] -> Lazy.Text
fragmentSpread formatter name directives'
    = "..." <> Lazy.Text.fromStrict name
    <> optempty (directives formatter) directives'

inlineFragment ::
    Formatter ->
    Maybe Full.TypeCondition ->
    [Full.Directive] ->
    Full.SelectionSet ->
    Lazy.Text
inlineFragment formatter tc dirs sels = "... on "
    <> Lazy.Text.fromStrict (fold tc)
    <> directives formatter dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

fragmentDefinition :: Formatter -> Full.FragmentDefinition -> Lazy.Text
fragmentDefinition formatter (Full.FragmentDefinition name tc dirs sels)
    = "fragment " <> Lazy.Text.fromStrict name
    <> " on " <> Lazy.Text.fromStrict tc
    <> optempty (directives formatter) dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

-- * Miscellaneous

-- | Converts a 'Full.Directive' into a string.
directive :: Formatter -> Full.Directive -> Lazy.Text
directive formatter (Full.Directive name args)
    = "@" <> Lazy.Text.fromStrict name <> optempty (arguments formatter) args

directives :: Formatter -> [Full.Directive] -> Lazy.Text
directives Minified = spaces (directive Minified)
directives formatter = Lazy.Text.cons ' ' . spaces (directive formatter)

-- | Converts a 'Full.Value' into a string.
value :: Formatter -> Full.Value -> Lazy.Text
value _ (Full.Variable x) = variable x
value _ (Full.Int x) = Builder.toLazyText $ decimal x
value _ (Full.Float x) = Builder.toLazyText $ realFloat x
value _ (Full.Boolean  x) = booleanValue x
value _ Full.Null = "null"
value formatter (Full.String string) = stringValue formatter string
value _ (Full.Enum x) = Lazy.Text.fromStrict x
value formatter (Full.List x) = listValue formatter x
value formatter (Full.Object x) = objectValue formatter x

booleanValue :: Bool -> Lazy.Text
booleanValue True  = "true"
booleanValue False = "false"

quote :: Builder.Builder
quote = Builder.singleton '\"'

oneLine :: Text -> Builder
oneLine string = quote <> Text.foldr (mappend . escape) quote string

stringValue :: Formatter -> Text -> Lazy.Text
stringValue Minified string = Builder.toLazyText
    $ quote <> Text.foldr (mappend . escape) quote string
stringValue (Pretty indentation) string =
  if hasEscaped string
  then stringValue Minified string
  else Builder.toLazyText $ encoded lines'
    where
      isWhiteSpace char = char == ' ' || char == '\t'
      isNewline char = char == '\n' || char == '\r'
      hasEscaped = Text.any (not . isAllowed)
      isAllowed char =
          char == '\t' || isNewline char || (char >= '\x0020' && char /= '\x007F')

      tripleQuote = Builder.fromText "\"\"\""
      start = tripleQuote <> Builder.singleton '\n'
      end = Builder.fromLazyText (indent indentation) <> tripleQuote

      strip = Text.dropWhile isWhiteSpace . Text.dropWhileEnd isWhiteSpace
      lines' = map Builder.fromText $ Text.split isNewline (Text.replace "\r\n" "\n" $ strip string)
      encoded [] = oneLine string
      encoded [_] = oneLine string
      encoded lines'' = start <> transformLines lines'' <> end
      transformLines = foldr ((\line acc -> line <> Builder.singleton '\n' <> acc) . transformLine) mempty
      transformLine line =
        if Lazy.Text.null (Builder.toLazyText line)
        then line
        else Builder.fromLazyText (indent (indentation + 1)) <> line

escape :: Char -> Builder
escape char'
    | char' == '\\' = Builder.fromString "\\\\"
    | char' == '\"' = Builder.fromString "\\\""
    | char' == '\b' = Builder.fromString "\\b"
    | char' == '\f' = Builder.fromString "\\f"
    | char' == '\n' = Builder.fromString "\\n"
    | char' == '\r' = Builder.fromString "\\r"
    | char' == '\t' = Builder.fromString "\\t"
    | char' < '\x0010' = unicode  "\\u000" char'
    | char' < '\x0020' = unicode "\\u00" char'
    | otherwise = Builder.singleton char'
  where
    unicode prefix = mappend (Builder.fromString prefix) . (hexadecimal . ord)

listValue :: Formatter -> [Full.Value] -> Lazy.Text
listValue formatter = bracketsCommas formatter $ value formatter

objectValue :: Formatter -> [Full.ObjectField] -> Lazy.Text
objectValue formatter = intercalate $ objectField formatter
  where
    intercalate f
        = braces
        . Lazy.Text.intercalate (eitherFormat formatter ", " ",")
        . fmap f

objectField :: Formatter -> Full.ObjectField -> Lazy.Text
objectField formatter (Full.ObjectField name value') =
    Lazy.Text.fromStrict name <> colon formatter <> value formatter value'

-- | Converts a 'Full.Type' a type into a string.
type' :: Full.Type -> Lazy.Text
type' (Full.TypeNamed   x) = Lazy.Text.fromStrict x
type' (Full.TypeList    x) = listType x
type' (Full.TypeNonNull x) = nonNullType x

listType :: Full.Type -> Lazy.Text
listType x = brackets (type' x)

nonNullType :: Full.NonNullType -> Lazy.Text
nonNullType (Full.NonNullTypeNamed x) = Lazy.Text.fromStrict x <> "!"
nonNullType (Full.NonNullTypeList  x) = listType x <> "!"

-- * Internal

between :: Char -> Char -> Lazy.Text -> Lazy.Text
between open close = Lazy.Text.cons open . (`Lazy.Text.snoc` close)

parens :: Lazy.Text -> Lazy.Text
parens = between '(' ')'

brackets :: Lazy.Text -> Lazy.Text
brackets = between '[' ']'

braces :: Lazy.Text -> Lazy.Text
braces = between '{' '}'

spaces :: forall a. (a -> Lazy.Text) -> [a] -> Lazy.Text
spaces f = Lazy.Text.intercalate "\SP" . fmap f

parensCommas :: forall a. Formatter -> (a -> Lazy.Text) -> [a] -> Lazy.Text
parensCommas formatter f
    = parens
    . Lazy.Text.intercalate (eitherFormat formatter ", " ",")
    . fmap f

bracketsCommas :: Formatter -> (a -> Lazy.Text) -> [a] -> Lazy.Text
bracketsCommas formatter f
    = brackets
    . Lazy.Text.intercalate (eitherFormat formatter ", " ",")
    . fmap f

bracesList :: forall a. Formatter -> (a -> Lazy.Text) -> [a] -> Lazy.Text
bracesList (Pretty intendation) f xs
    = Lazy.Text.snoc (Lazy.Text.intercalate "\n" content) '\n'
    <> (Lazy.Text.snoc $ Lazy.Text.replicate (fromIntegral intendation) "  ") '}'
  where
    content = "{" : fmap f xs
bracesList Minified f xs = braces $ Lazy.Text.intercalate "," $ fmap f xs

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs

eitherFormat :: forall a. Formatter -> a -> a -> a
eitherFormat (Pretty _) x _ = x
eitherFormat Minified _ x = x
