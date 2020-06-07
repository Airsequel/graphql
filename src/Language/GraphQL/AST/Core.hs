-- | This is the AST meant to be executed.
module Language.GraphQL.AST.Core
    ( Arguments(..)
    ) where

import Data.HashMap.Strict (HashMap)
import Language.GraphQL.AST (Name)
import Language.GraphQL.Type.Definition

-- | Argument list.
newtype Arguments = Arguments (HashMap Name Value)
    deriving (Eq, Show)

instance Semigroup Arguments where
    (Arguments x) <> (Arguments y) = Arguments $ x <> y

instance Monoid Arguments where
    mempty = Arguments mempty

