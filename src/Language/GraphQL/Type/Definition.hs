{-# LANGUAGE OverloadedStrings #-}

-- | Types that can be used as both input and output types.
module Language.GraphQL.Type.Definition
    ( Arguments(..)
    , Directive(..)
    , EnumType(..)
    , EnumValue(..)
    , ScalarType(..)
    , Subs
    , Value(..)
    , boolean
    , float
    , id
    , int
    , selection
    , string
    ) where

import Data.Int (Int32)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Language.GraphQL.AST (Name)
import Prelude hiding (id)

-- | Represents accordingly typed GraphQL values.
data Value
    = Int Int32
    | Float Double -- ^ GraphQL Float is double precision.
    | String Text
    | Boolean Bool
    | Null
    | Enum Name
    | List [Value] -- ^ Arbitrary nested list.
    | Object (HashMap Name Value)
    deriving (Eq, Show)

instance IsString Value where
    fromString = String . fromString

-- | Contains variables for the query. The key of the map is a variable name,
-- and the value is the variable value.
type Subs = HashMap Name Value

-- | Argument list.
newtype Arguments = Arguments (HashMap Name Value)
    deriving (Eq, Show)

instance Semigroup Arguments where
    (Arguments x) <> (Arguments y) = Arguments $ x <> y

instance Monoid Arguments where
    mempty = Arguments mempty

-- | Scalar type definition.
--
-- The leaf values of any request and input values to arguments are Scalars (or
-- Enums) .
data ScalarType = ScalarType Name (Maybe Text)

instance Eq ScalarType where
    (ScalarType this _) == (ScalarType that _) = this == that

instance Show ScalarType where
    show (ScalarType typeName _) = Text.unpack typeName

-- | Enum type definition.
--
-- Some leaf values of requests and input values are Enums. GraphQL serializes
-- Enum values as strings, however internally Enums can be represented by any
-- kind of type, often integers.
data EnumType = EnumType Name (Maybe Text) (HashMap Name EnumValue)

instance Eq EnumType where
    (EnumType this _ _) == (EnumType that _ _) = this == that

instance Show EnumType where
    show (EnumType typeName _ _) = Text.unpack typeName

-- | Enum value is a single member of an 'EnumType'.
newtype EnumValue = EnumValue (Maybe Text)

-- | The @String@ scalar type represents textual data, represented as UTF-8
-- character sequences. The String type is most often used by GraphQL to
-- represent free-form human-readable text.
string :: ScalarType
string = ScalarType "String" (Just description)
  where
    description =
        "The `String` scalar type represents textual data, represented as \
        \UTF-8 character sequences. The String type is most often used by \
        \GraphQL to represent free-form human-readable text."

-- | The @Boolean@ scalar type represents @true@ or @false@.
boolean :: ScalarType
boolean = ScalarType "Boolean" (Just description)
  where
    description = "The `Boolean` scalar type represents `true` or `false`."

-- | The @Int@ scalar type represents non-fractional signed whole numeric
-- values. Int can represent values between \(-2^{31}\) and \(2^{31 - 1}\).
int :: ScalarType
int = ScalarType "Int" (Just description)
  where
    description =
        "The `Int` scalar type represents non-fractional signed whole numeric \
        \values. Int can represent values between -(2^31) and 2^31 - 1."

-- | The @Float@ scalar type represents signed double-precision fractional
-- values as specified by
-- [IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point).
float :: ScalarType
float = ScalarType "Float" (Just description)
  where
    description =
        "The `Float` scalar type represents signed double-precision fractional \
        \values as specified by \
        \[IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point)."

-- | The @ID@ scalar type represents a unique identifier, often used to refetch
-- an object or as key for a cache. The ID type appears in a JSON response as a
-- String; however, it is not intended to be human-readable. When expected as an
-- input type, any string (such as @"4"@) or integer (such as @4@) input value
-- will be accepted as an ID.
id :: ScalarType
id = ScalarType "ID" (Just description)
  where
    description =
        "The `ID` scalar type represents a unique identifier, often used to \
        \refetch an object or as key for a cache. The ID type appears in a \
        \JSON response as a String; however, it is not intended to be \
        \human-readable. When expected as an input type, any string (such as \
        \`\"4\"`) or integer (such as `4`) input value will be accepted as an ID."

-- | Directive.
data Directive = Directive Name Arguments
    deriving (Eq, Show)

-- | Directive processing status.
data Status
    = Skip -- ^ Skip the selection and stop directive processing
    | Include Directive -- ^ The directive was processed, try other handlers
    | Continue Directive -- ^ Directive handler mismatch, try other handlers

-- | Takes a list of directives, handles supported directives and excludes them
--   from the result. If the selection should be skipped, returns 'Nothing'.
selection :: [Directive] -> Maybe [Directive]
selection = foldr go (Just [])
  where
    go directive' directives' =
        case (skip . include) (Continue directive') of
            (Include _) -> directives'
            Skip -> Nothing
            (Continue x) -> (x :) <$> directives'

handle :: (Directive -> Status) -> Status -> Status
handle _ Skip = Skip
handle handler (Continue directive) = handler directive
handle handler (Include directive) = handler directive

-- * Directive implementations

skip :: Status -> Status
skip = handle skip'
  where
    skip' directive'@(Directive "skip" (Arguments arguments)) =
        case HashMap.lookup "if" arguments of
            (Just (Boolean True)) -> Skip
            _ -> Include directive'
    skip' directive' = Continue directive'

include :: Status -> Status
include = handle include'
  where
    include' directive'@(Directive "include" (Arguments arguments)) =
        case HashMap.lookup "if" arguments of
            (Just (Boolean True)) -> Include directive'
            _ -> Skip
    include' directive' = Continue directive'
