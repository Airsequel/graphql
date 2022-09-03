{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Input types and values.
--
-- This module is intended to be imported qualified, to avoid name clashes
-- with 'Language.GraphQL.Type.Out'.
module Language.GraphQL.Type.In
    ( Argument(..)
    , Arguments
    , InputField(..)
    , InputObjectType(..)
    , Type(..)
    , isNonNullType
    , pattern EnumBaseType
    , pattern ListBaseType
    , pattern InputObjectBaseType
    , pattern ScalarBaseType
    ) where

import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.GraphQL.Type.Definition as Definition

-- | Single field of an 'InputObjectType'.
data InputField = InputField (Maybe Text) Type (Maybe Definition.Value)

-- | Input object type definition.
--
-- An input object defines a structured collection of fields which may be
-- supplied to a field argument.
data InputObjectType = InputObjectType
    Key.Key (Maybe Text) (KeyMap InputField)

instance Eq InputObjectType where
    (InputObjectType this _ _) == (InputObjectType that _ _) = this == that

instance Show InputObjectType where
    show (InputObjectType typeName _ _) = Key.toString typeName

-- | These types may be used as input types for arguments and directives.
--
-- GraphQL distinguishes between "wrapping" and "named" types. Each wrapping
-- type can wrap other wrapping or named types. Wrapping types are lists and
-- Non-Null types (named types are nullable by default).
data Type
    = NamedScalarType Definition.ScalarType
    | NamedEnumType Definition.EnumType
    | NamedInputObjectType InputObjectType
    | ListType Type
    | NonNullScalarType Definition.ScalarType
    | NonNullEnumType Definition.EnumType
    | NonNullInputObjectType InputObjectType
    | NonNullListType Type
    deriving Eq

instance Show Type where
    show (NamedScalarType scalarType) = show scalarType
    show (NamedEnumType enumType) = show enumType
    show (NamedInputObjectType inputObjectType) = show inputObjectType
    show (ListType baseType) = concat ["[", show baseType, "]"]
    show (NonNullScalarType scalarType) = '!' : show scalarType
    show (NonNullEnumType enumType) = '!' : show enumType
    show (NonNullInputObjectType inputObjectType) = '!' : show inputObjectType
    show (NonNullListType baseType) = concat ["![", show baseType, "]"]

-- | Field argument definition.
data Argument = Argument (Maybe Text) Type (Maybe Definition.Value)

-- | Field argument definitions.
type Arguments = KeyMap Argument

-- | Matches either 'NamedScalarType' or 'NonNullScalarType'.
pattern ScalarBaseType :: Definition.ScalarType -> Type
pattern ScalarBaseType scalarType <- (isScalarType -> Just scalarType)

-- | Matches either 'NamedEnumType' or 'NonNullEnumType'.
pattern EnumBaseType :: Definition.EnumType -> Type
pattern EnumBaseType enumType <- (isEnumType -> Just enumType)

-- | Matches either 'NamedInputObjectType' or 'NonNullInputObjectType'.
pattern InputObjectBaseType :: InputObjectType -> Type
pattern InputObjectBaseType objectType <- (isInputObjectType -> Just objectType)

-- | Matches either 'ListType' or 'NonNullListType'.
pattern ListBaseType :: Type -> Type
pattern ListBaseType listType <- (isListType -> Just listType)

{-# COMPLETE EnumBaseType, ListBaseType, InputObjectBaseType, ScalarBaseType #-}

isScalarType :: Type -> Maybe Definition.ScalarType
isScalarType (NamedScalarType inputType) = Just inputType
isScalarType (NonNullScalarType inputType) = Just inputType
isScalarType _ = Nothing

isInputObjectType :: Type -> Maybe InputObjectType
isInputObjectType (NamedInputObjectType inputType) = Just inputType
isInputObjectType (NonNullInputObjectType inputType) = Just inputType
isInputObjectType _ = Nothing

isEnumType :: Type -> Maybe Definition.EnumType
isEnumType (NamedEnumType inputType) = Just inputType
isEnumType (NonNullEnumType inputType) = Just inputType
isEnumType _ = Nothing

isListType :: Type -> Maybe Type
isListType (ListType inputType) = Just inputType
isListType (NonNullListType inputType) = Just inputType
isListType _ = Nothing

-- | Checks whether the given input type is a non-null type.
isNonNullType :: Type -> Bool
isNonNullType (NonNullScalarType _) = True
isNonNullType (NonNullEnumType _) = True
isNonNullType (NonNullInputObjectType _) = True
isNonNullType (NonNullListType _) = True
isNonNullType _ = False
