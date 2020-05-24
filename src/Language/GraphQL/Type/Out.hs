{-# LANGUAGE OverloadedStrings #-}

-- | This module is intended to be imported qualified, to avoid name clashes
-- with 'Language.GraphQL.Type.In'.
module Language.GraphQL.Type.Out
    ( Value(..)
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int32)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Language.GraphQL.AST.Document (Name)
import Language.GraphQL.Trans

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
    show (Float float) = "Float " ++ show float
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
