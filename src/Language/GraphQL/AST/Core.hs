-- | This is the AST meant to be executed.
module Language.GraphQL.AST.Core
    ( Alias
    , Arguments(..)
    , Directive(..)
    , Field(..)
    , Fragment(..)
    , Name
    , Operation(..)
    , Selection(..)
    , TypeCondition
    ) where

import Data.HashMap.Strict (HashMap)
import Data.Sequence (Seq)
import Data.Text (Text)
import Language.GraphQL.AST (Alias, Name, TypeCondition)
import qualified Language.GraphQL.Type.In as In

-- | GraphQL has 3 operation types: queries, mutations and subscribtions.
--
-- Currently only queries and mutations are supported.
data Operation
    = Query (Maybe Text) (Seq Selection)
    | Mutation (Maybe Text) (Seq Selection)
    deriving (Eq, Show)

-- | Single GraphQL field.
data Field
    = Field (Maybe Alias) Name Arguments (Seq Selection)
    deriving (Eq, Show)

-- | Argument list.
newtype Arguments = Arguments (HashMap Name In.Value)
    deriving (Eq, Show)

instance Semigroup Arguments where
    (Arguments x) <> (Arguments y) = Arguments $ x <> y

instance Monoid Arguments where
    mempty = Arguments mempty

-- | Directive.
data Directive = Directive Name Arguments
    deriving (Eq, Show)

-- | Represents fragments and inline fragments.
data Fragment
    = Fragment TypeCondition (Seq Selection)
    deriving (Eq, Show)

-- | Single selection element.
data Selection
    = SelectionFragment Fragment
    | SelectionField Field
    deriving (Eq, Show)
