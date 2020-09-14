{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
    executableDefinition (ExecutableDefinition executableDefinition') acc =
        definition formatter executableDefinition' : acc
    executableDefinition _ acc = acc

-- | Converts a t'ExecutableDefinition' into a string.
definition :: Formatter -> ExecutableDefinition -> Lazy.Text
definition formatter x
    | Pretty _ <- formatter = Lazy.Text.snoc (encodeDefinition x) '\n'
    | Minified <- formatter = encodeDefinition x
  where
    encodeDefinition (DefinitionOperation operation)
        = operationDefinition formatter operation
    encodeDefinition (DefinitionFragment fragment)
        = fragmentDefinition formatter fragment

-- | Converts a 'OperationDefinition into a string.
operationDefinition :: Formatter -> OperationDefinition -> Lazy.Text
operationDefinition formatter = \case
    SelectionSet sels _ -> selectionSet formatter sels
    OperationDefinition Query name vars dirs sels _ ->
        "query " <> node formatter name vars dirs sels
    OperationDefinition Mutation name vars dirs sels _ ->
        "mutation " <> node formatter name vars dirs sels
    OperationDefinition Subscription name vars dirs sels _ ->
        "subscription " <> node formatter name vars dirs sels

-- | Converts a Query or Mutation into a string.
node :: Formatter ->
    Maybe Name ->
    [VariableDefinition] ->
    [Directive] ->
    SelectionSet ->
    Lazy.Text
node formatter name vars dirs sels
    = Lazy.Text.fromStrict (fold name)
    <> optempty (variableDefinitions formatter) vars
    <> optempty (directives formatter) dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

variableDefinitions :: Formatter -> [VariableDefinition] -> Lazy.Text
variableDefinitions formatter
    = parensCommas formatter $ variableDefinition formatter

variableDefinition :: Formatter -> VariableDefinition -> Lazy.Text
variableDefinition formatter (VariableDefinition var ty defaultValue')
    = variable var
    <> eitherFormat formatter ": " ":"
    <> type' ty
    <> maybe mempty (defaultValue formatter) defaultValue'

defaultValue :: Formatter -> ConstValue -> Lazy.Text
defaultValue formatter val
    = eitherFormat formatter " = " "="
    <> value formatter (fromConstValue val)

variable :: Name -> Lazy.Text
variable var = "$" <> Lazy.Text.fromStrict var

selectionSet :: Formatter -> SelectionSet -> Lazy.Text
selectionSet formatter
    = bracesList formatter (selection formatter)
    . NonEmpty.toList

selectionSetOpt :: Formatter -> SelectionSetOpt -> Lazy.Text
selectionSetOpt formatter = bracesList formatter $ selection formatter

indentSymbol :: Lazy.Text
indentSymbol = "  "

indent :: (Integral a) => a -> Lazy.Text
indent indentation = Lazy.Text.replicate (fromIntegral indentation) indentSymbol

selection :: Formatter -> Selection -> Lazy.Text
selection formatter = Lazy.Text.append indent' . encodeSelection
  where
    encodeSelection (FieldSelection fieldSelection) =
        field incrementIndent fieldSelection
    encodeSelection (InlineFragmentSelection fragmentSelection) =
        inlineFragment incrementIndent fragmentSelection
    encodeSelection (FragmentSpreadSelection fragmentSelection) =
        fragmentSpread incrementIndent fragmentSelection
    incrementIndent
        | Pretty indentation <- formatter = Pretty $ indentation + 1
        | otherwise = Minified
    indent'
        | Pretty indentation <- formatter = indent $ indentation + 1
        | otherwise = ""

colon :: Formatter -> Lazy.Text
colon formatter = eitherFormat formatter ": " ":"

-- | Converts Field into a string.
field :: Formatter -> Field -> Lazy.Text
field formatter (Field alias name args dirs set _)
    = optempty prependAlias (fold alias)
    <> Lazy.Text.fromStrict name
    <> optempty (arguments formatter) args
    <> optempty (directives formatter) dirs
    <> optempty selectionSetOpt' set
  where
    prependAlias aliasName = Lazy.Text.fromStrict aliasName <>  colon formatter
    selectionSetOpt' = (eitherFormat formatter " " "" <>)
        . selectionSetOpt formatter

arguments :: Formatter -> [Argument] -> Lazy.Text
arguments formatter = parensCommas formatter $ argument formatter

argument :: Formatter -> Argument -> Lazy.Text
argument formatter (Argument name value' _)
    = Lazy.Text.fromStrict name
    <> colon formatter
    <> value formatter value'

-- * Fragments

fragmentSpread :: Formatter -> FragmentSpread -> Lazy.Text
fragmentSpread formatter (FragmentSpread name directives' _)
    = "..." <> Lazy.Text.fromStrict name
    <> optempty (directives formatter) directives'

inlineFragment :: Formatter -> InlineFragment -> Lazy.Text
inlineFragment formatter (InlineFragment typeCondition directives' selections _)
    = "... on "
    <> Lazy.Text.fromStrict (fold typeCondition)
    <> directives formatter directives'
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter selections

fragmentDefinition :: Formatter -> FragmentDefinition -> Lazy.Text
fragmentDefinition formatter (FragmentDefinition name tc dirs sels _)
    = "fragment " <> Lazy.Text.fromStrict name
    <> " on " <> Lazy.Text.fromStrict tc
    <> optempty (directives formatter) dirs
    <> eitherFormat formatter " " mempty
    <> selectionSet formatter sels

-- * Miscellaneous

-- | Converts a 'Directive' into a string.
directive :: Formatter -> Directive -> Lazy.Text
directive formatter (Directive name args)
    = "@" <> Lazy.Text.fromStrict name <> optempty (arguments formatter) args

directives :: Formatter -> [Directive] -> Lazy.Text
directives Minified = spaces (directive Minified)
directives formatter = Lazy.Text.cons ' ' . spaces (directive formatter)

-- | Converts a 'Value' into a string.
value :: Formatter -> Value -> Lazy.Text
value _ (Variable x) = variable x
value _ (Int x) = Builder.toLazyText $ decimal x
value _ (Float x) = Builder.toLazyText $ realFloat x
value _ (Boolean  x) = booleanValue x
value _ Null = "null"
value formatter (String string) = stringValue formatter string
value _ (Enum x) = Lazy.Text.fromStrict x
value formatter (List x) = listValue formatter x
value formatter (Object x) = objectValue formatter x

fromConstValue :: ConstValue -> Value
fromConstValue (ConstInt x) = Int x
fromConstValue (ConstFloat x) = Float x
fromConstValue (ConstBoolean  x) = Boolean x
fromConstValue ConstNull = Null
fromConstValue (ConstString string) = String string
fromConstValue (ConstEnum x) = Enum x
fromConstValue (ConstList x) = List $ fromConstValue <$> x
fromConstValue (ConstObject x) = Object $ fromConstObjectField <$> x
  where
    fromConstObjectField (ObjectField key value') =
        ObjectField key $ fromConstValue value'

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
      newline = Builder.singleton '\n'

      strip = Text.dropWhile isWhiteSpace . Text.dropWhileEnd isWhiteSpace
      lines' = map Builder.fromText $ Text.split isNewline (Text.replace "\r\n" "\n" $ strip string)
      encoded [] = oneLine string
      encoded [_] = oneLine string
      encoded lines'' = tripleQuote <> newline
        <> transformLines lines''
        <> Builder.fromLazyText (indent indentation) <> tripleQuote
      transformLines = foldr transformLine mempty
      transformLine "" acc = newline <> acc
      transformLine line' acc
            = Builder.fromLazyText (indent (indentation + 1))
            <> line' <> newline <> acc

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

listValue :: Formatter -> [Value] -> Lazy.Text
listValue formatter = bracketsCommas formatter $ value formatter

objectValue :: Formatter -> [ObjectField Value] -> Lazy.Text
objectValue formatter = intercalate $ objectField formatter
  where
    intercalate f
        = braces
        . Lazy.Text.intercalate (eitherFormat formatter ", " ",")
        . fmap f

objectField :: Formatter -> ObjectField Value -> Lazy.Text
objectField formatter (ObjectField name value') =
    Lazy.Text.fromStrict name <> colon formatter <> value formatter value'

-- | Converts a 'Type' a type into a string.
type' :: Type -> Lazy.Text
type' (TypeNamed   x) = Lazy.Text.fromStrict x
type' (TypeList    x) = listType x
type' (TypeNonNull x) = nonNullType x

listType :: Type -> Lazy.Text
listType x = brackets (type' x)

nonNullType :: NonNullType -> Lazy.Text
nonNullType (NonNullTypeNamed x) = Lazy.Text.fromStrict x <> "!"
nonNullType (NonNullTypeList  x) = listType x <> "!"

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
