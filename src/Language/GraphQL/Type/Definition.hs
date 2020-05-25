{-# LANGUAGE OverloadedStrings #-}

-- | Types that can be used as both input and output types.
module Language.GraphQL.Type.Definition
    ( EnumType(..)
    , ScalarType(..)
    , boolean
    , float
    , id
    , int
    , string
    ) where

import Data.Set (Set)
import Data.Text (Text)
import Language.GraphQL.AST.Document (Name)
import Prelude hiding (id)

-- | Scalar type definition.
--
-- The leaf values of any request and input values to arguments are Scalars (or
-- Enums) .
data ScalarType = ScalarType Name (Maybe Text)

-- | Enum type definition.
--
-- Some leaf values of requests and input values are Enums. GraphQL serializes
-- Enum values as strings, however internally Enums can be represented by any
-- kind of type, often integers.
data EnumType = EnumType Name (Maybe Text) (Set Text)

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
