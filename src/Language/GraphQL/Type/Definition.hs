{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Types representing GraphQL type system.
module Language.GraphQL.Type.Definition
    ( Argument(..)
    , EnumType(..)
    , Field(..)
    , InputField(..)
    , InputObjectType(..)
    , InputType(..)
    , ObjectType(..)
    , OutputType(..)
    , ScalarType(..)
    , TypeDefinition(..)
    , pattern EnumInputTypeDefinition
    , pattern ListInputTypeDefinition
    , pattern ObjectInputTypeDefinition
    , pattern ScalarInputTypeDefinition
    , pattern EnumOutputTypeDefinition
    , pattern ListOutputTypeDefinition
    , pattern ObjectOutputTypeDefinition
    , pattern ScalarOutputTypeDefinition
    , boolean
    , float
    , id
    , int
    , string
    ) where

import Data.HashMap.Strict (HashMap)
import Data.Set (Set)
import Data.Text (Text)
import Language.GraphQL.AST.Document (Name)
import Language.GraphQL.Trans
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Prelude hiding (id)

-- | Object type definition.
--
--   Almost all of the GraphQL types you define will be object types. Object
--   types have a name, but most importantly describe their fields.
data ObjectType m = ObjectType Name (Maybe Text) (HashMap Name (Field m))

-- | Output object field definition.
data Field m = Field
    (Maybe Text) -- ^ Description.
    (OutputType m) -- ^ Field type.
    (HashMap Name Argument) -- ^ Arguments.
    (ActionT m (Out.Value m)) -- ^ Resolver.

-- | Field argument definition.
data Argument = Argument (Maybe Text) InputType (Maybe In.Value)

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

-- | Single field of an 'InputObjectType'.
data InputField = InputField (Maybe Text) InputType (Maybe In.Value)

-- | Input object type definition.
--
-- An input object defines a structured collection of fields which may be
-- supplied to a field argument.
data InputObjectType = InputObjectType
    Name (Maybe Text) (HashMap Name InputField)

-- | These types may be used as input types for arguments and directives.
data InputType
    = ScalarInputType ScalarType
    | EnumInputType EnumType
    | ObjectInputType InputObjectType
    | ListInputType InputType
    | NonNullScalarInputType ScalarType
    | NonNullEnumInputType EnumType
    | NonNullObjectInputType InputObjectType
    | NonNullListInputType InputType

-- | These types may be used as output types as the result of fields.
data OutputType m
    = ScalarOutputType ScalarType
    | EnumOutputType EnumType
    | ObjectOutputType (ObjectType m)
    | ListOutputType (OutputType m)
    | NonNullScalarOutputType ScalarType
    | NonNullEnumOutputType EnumType
    | NonNullObjectOutputType (ObjectType m)
    | NonNullListOutputType (OutputType m)

-- | These are all of the possible kinds of types.
data TypeDefinition m
    = ScalarTypeDefinition ScalarType
    | EnumTypeDefinition EnumType
    | ObjectTypeDefinition (ObjectType m)
    | InputObjectTypeDefinition InputObjectType

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

-- | Matches either 'ScalarInputType' or 'NonNullScalarInputType'.
pattern ScalarInputTypeDefinition :: ScalarType -> InputType
pattern ScalarInputTypeDefinition scalarType <-
    (isScalarInputType -> Just scalarType)

-- | Matches either 'EnumInputType' or 'NonNullEnumInputType'.
pattern EnumInputTypeDefinition :: EnumType -> InputType
pattern EnumInputTypeDefinition enumType <-
    (isEnumInputType -> Just enumType)

-- | Matches either 'ObjectInputType' or 'NonNullObjectInputType'.
pattern ObjectInputTypeDefinition :: InputObjectType -> InputType
pattern ObjectInputTypeDefinition objectType <-
    (isObjectInputType -> Just objectType)

-- | Matches either 'ListInputType' or 'NonNullListInputType'.
pattern ListInputTypeDefinition :: InputType -> InputType
pattern ListInputTypeDefinition listType <-
    (isListInputType -> Just listType)

{-# COMPLETE EnumInputTypeDefinition
    , ListInputTypeDefinition
    , ObjectInputTypeDefinition
    , ScalarInputTypeDefinition
    #-}

-- | Matches either 'ScalarOutputType' or 'NonNullScalarOutputType'.
pattern ScalarOutputTypeDefinition :: forall m. ScalarType -> OutputType m
pattern ScalarOutputTypeDefinition scalarType <-
    (isScalarOutputType -> Just scalarType)

-- | Matches either 'EnumOutputType' or 'NonNullEnumOutputType'.
pattern EnumOutputTypeDefinition :: forall m. EnumType -> OutputType m
pattern EnumOutputTypeDefinition enumType <-
    (isEnumOutputType -> Just enumType)

-- | Matches either 'ObjectOutputType' or 'NonNullObjectOutputType'.
pattern ObjectOutputTypeDefinition :: forall m. ObjectType m -> OutputType m
pattern ObjectOutputTypeDefinition objectType <-
    (isObjectOutputType -> Just objectType)

-- | Matches either 'ListOutputType' or 'NonNullListOutputType'.
pattern ListOutputTypeDefinition :: forall m. OutputType m -> OutputType m
pattern ListOutputTypeDefinition listType <-
    (isListOutputType -> Just listType)

{-# COMPLETE ScalarOutputTypeDefinition
    , EnumOutputTypeDefinition
    , ObjectOutputTypeDefinition
    , ListOutputTypeDefinition
    #-}

isScalarInputType :: InputType -> Maybe ScalarType
isScalarInputType (ScalarInputType inputType) = Just inputType
isScalarInputType (NonNullScalarInputType inputType) = Just inputType
isScalarInputType _ = Nothing

isObjectInputType :: InputType -> Maybe InputObjectType
isObjectInputType (ObjectInputType inputType) = Just inputType
isObjectInputType (NonNullObjectInputType inputType) = Just inputType
isObjectInputType _ = Nothing

isEnumInputType :: InputType -> Maybe EnumType
isEnumInputType (EnumInputType inputType) = Just inputType
isEnumInputType (NonNullEnumInputType inputType) = Just inputType
isEnumInputType _ = Nothing

isListInputType :: InputType -> Maybe InputType
isListInputType (ListInputType inputType) = Just inputType
isListInputType (NonNullListInputType inputType) = Just inputType
isListInputType _ = Nothing

isScalarOutputType :: forall m. OutputType m -> Maybe ScalarType
isScalarOutputType (ScalarOutputType outputType) = Just outputType
isScalarOutputType (NonNullScalarOutputType outputType) = Just outputType
isScalarOutputType _ = Nothing

isObjectOutputType :: forall m. OutputType m -> Maybe (ObjectType m)
isObjectOutputType (ObjectOutputType outputType) = Just outputType
isObjectOutputType (NonNullObjectOutputType outputType) = Just outputType
isObjectOutputType _ = Nothing

isEnumOutputType :: forall m. OutputType m -> Maybe EnumType
isEnumOutputType (EnumOutputType outputType) = Just outputType
isEnumOutputType (NonNullEnumOutputType outputType) = Just outputType
isEnumOutputType _ = Nothing

isListOutputType :: forall m. OutputType m -> Maybe (OutputType m)
isListOutputType (ListOutputType outputType) = Just outputType
isListOutputType (NonNullListOutputType outputType) = Just outputType
isListOutputType _ = Nothing
