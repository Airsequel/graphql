{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Output types and values.
--
-- This module is intended to be imported qualified, to avoid name clashes
-- with 'Language.GraphQL.Type.In'.
module Language.GraphQL.Type.Out
    ( Field(..)
    , InterfaceType(..)
    , ObjectType(..)
    , Type(..)
    , UnionType(..)
    , Value(..)
    , isNonNullType
    , pattern EnumBaseType
    , pattern InterfaceBaseType
    , pattern ListBaseType
    , pattern ObjectBaseType
    , pattern ScalarBaseType
    , pattern UnionBaseType
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int32)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Language.GraphQL.AST.Document (Name)
import Language.GraphQL.Trans
import Language.GraphQL.Type.Definition
import qualified Language.GraphQL.Type.In as In
--
-- | Object type definition.
--
--   Almost all of the GraphQL types you define will be object types. Object
--   types have a name, but most importantly describe their fields.
data ObjectType m = ObjectType
    Name (Maybe Text) [InterfaceType m] (HashMap Name (Field m))

-- | Interface Type Definition.
--
-- When a field can return one of a heterogeneous set of types, a Interface type
-- is used to describe what types are possible, and what fields are in common
-- across all types.
data InterfaceType m = InterfaceType
    Name (Maybe Text) [InterfaceType m] (HashMap Name (Field m))

-- | Union Type Definition.
--
-- When a field can return one of a heterogeneous set of types, a Union type is
-- used to describe what types are possible.
data UnionType m = UnionType Name (Maybe Text) [ObjectType m]

-- | Output object field definition.
data Field m = Field
    (Maybe Text) -- ^ Description.
    (Type m) -- ^ Field type.
    (HashMap Name In.Argument) -- ^ Arguments.
    (ActionT m (Value m)) -- ^ Resolver.

-- | These types may be used as output types as the result of fields.
data Type m
    = NamedScalarType ScalarType
    | NamedEnumType EnumType
    | NamedObjectType (ObjectType m)
    | NamedInterfaceType (InterfaceType m)
    | NamedUnionType (UnionType m)
    | ListType (Type m)
    | NonNullScalarType ScalarType
    | NonNullEnumType EnumType
    | NonNullObjectType (ObjectType m)
    | NonNullInterfaceType (InterfaceType m)
    | NonNullUnionType (UnionType m)
    | NonNullListType (Type m)

-- | GraphQL distinguishes between "wrapping" and "named" types. Each wrapping
-- type can wrap other wrapping or named types. Wrapping types are lists and
-- Non-Null types (named types are nullable by default).
--
-- This 'Value' type doesn\'t reflect this distinction exactly but it is used
-- in the resolvers to take into account that the returned value can be nullable
-- or an (arbitrary nested) list.
data Value m
    = Int Int32
    | Float Double
    | String Text
    | Boolean Bool
    | Null
    | Enum Name
    | List [Value m] -- ^ Arbitrary nested list.
    | Object (HashMap Name (ActionT m (Value m)))

instance IsString (Value m) where
    fromString = String . fromString

instance Show (Value m) where
    show (Int integer) = "Int " ++ show integer
    show (Float float') = "Float " ++ show float'
    show (String text) = Text.unpack $ "String " <> text
    show (Boolean True) = "Boolean True"
    show (Boolean False) = "Boolean False"
    show Null = "Null"
    show (Enum enum) = Text.unpack $ "Enum " <> enum
    show (List list) = show list
    show (Object object) = Text.unpack
        $ "Object [" <> Text.intercalate ", " (HashMap.keys object) <> "]"

instance Eq (Value m) where
    (Int this) == (Int that) = this == that
    (Float this) == (Float that) = this == that
    (String this) == (String that) = this == that
    (Boolean this) == (Boolean that) = this == that
    (Enum this) == (Enum that) = this == that
    (List this) == (List that) = this == that
    (Object this) == (Object that) = HashMap.keys this == HashMap.keys that
    _ == _ = False

-- | Matches either 'NamedScalarType' or 'NonNullScalarType'.
pattern ScalarBaseType :: forall m. ScalarType -> Type m
pattern ScalarBaseType scalarType <- (isScalarType -> Just scalarType)

-- | Matches either 'NamedEnumType' or 'NonNullEnumType'.
pattern EnumBaseType :: forall m. EnumType -> Type m
pattern EnumBaseType enumType <- (isEnumType -> Just enumType)

-- | Matches either 'NamedObjectType' or 'NonNullObjectType'.
pattern ObjectBaseType :: forall m. ObjectType m -> Type m
pattern ObjectBaseType objectType <- (isObjectType -> Just objectType)

-- | Matches either 'NamedInterfaceType' or 'NonNullInterfaceType'.
pattern InterfaceBaseType :: forall m. InterfaceType m -> Type m
pattern InterfaceBaseType interfaceType <-
    (isInterfaceType -> Just interfaceType)

-- | Matches either 'NamedUnionType' or 'NonNullUnionType'.
pattern UnionBaseType :: forall m. UnionType m -> Type m
pattern UnionBaseType unionType <- (isUnionType -> Just unionType)

-- | Matches either 'ListType' or 'NonNullListType'.
pattern ListBaseType :: forall m. Type m -> Type m
pattern ListBaseType listType <- (isListType -> Just listType)

{-# COMPLETE ScalarBaseType
    , EnumBaseType
    , ObjectBaseType
    , ListBaseType
    , InterfaceBaseType
    , UnionBaseType
    #-}

isScalarType :: forall m. Type m -> Maybe ScalarType
isScalarType (NamedScalarType outputType) = Just outputType
isScalarType (NonNullScalarType outputType) = Just outputType
isScalarType _ = Nothing

isObjectType :: forall m. Type m -> Maybe (ObjectType m)
isObjectType (NamedObjectType outputType) = Just outputType
isObjectType (NonNullObjectType outputType) = Just outputType
isObjectType _ = Nothing

isEnumType :: forall m. Type m -> Maybe EnumType
isEnumType (NamedEnumType outputType) = Just outputType
isEnumType (NonNullEnumType outputType) = Just outputType
isEnumType _ = Nothing

isInterfaceType :: forall m. Type m -> Maybe (InterfaceType m)
isInterfaceType (NamedInterfaceType interfaceType) = Just interfaceType
isInterfaceType (NonNullInterfaceType interfaceType) = Just interfaceType
isInterfaceType _ = Nothing

isUnionType :: forall m. Type m -> Maybe (UnionType m)
isUnionType (NamedUnionType unionType) = Just unionType
isUnionType (NonNullUnionType unionType) = Just unionType
isUnionType _ = Nothing

isListType :: forall m. Type m -> Maybe (Type m)
isListType (ListType outputType) = Just outputType
isListType (NonNullListType outputType) = Just outputType
isListType _ = Nothing

-- | Checks whether the given output type is a non-null type.
isNonNullType :: forall m. Type m -> Bool
isNonNullType (NonNullScalarType _) = True
isNonNullType (NonNullEnumType _) = True
isNonNullType (NonNullObjectType _) = True
isNonNullType (NonNullInterfaceType _) = True
isNonNullType (NonNullUnionType _) = True
isNonNullType (NonNullListType _) = True
isNonNullType _ = False
