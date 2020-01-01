-- | This is the AST meant to be executed.
module Language.GraphQL.AST.Core
    ( Alias
    , Arguments(..)
    , Directive(..)
    , Document
    , Field(..)
    , Fragment(..)
    , Name
    , Operation(..)
    , Selection(..)
    , TypeCondition
    , Value(..)
    ) where

import Data.Int (Int32)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Sequence (Seq)
import Data.String (IsString(..))
import Data.Text (Text)
import Language.GraphQL.AST (Alias, Name, TypeCondition)

-- | GraphQL document is a non-empty list of operations.
type Document = NonEmpty Operation

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
newtype Arguments = Arguments (HashMap Name Value)
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

-- | Represents accordingly typed GraphQL values.
data Value
    = Int Int32
    | Float Double -- ^ GraphQL Float is double precision
    | String Text
    | Boolean Bool
    | Null
    | Enum Name
    | List [Value]
    | Object (HashMap Name Value)
    deriving (Eq, Show)

instance IsString Value where
    fromString = String . fromString
