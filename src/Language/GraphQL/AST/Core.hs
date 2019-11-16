-- | This is the AST meant to be executed.
module Language.GraphQL.AST.Core
    ( Alias
    , Argument(..)
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
    = Field (Maybe Alias) Name [Argument] (Seq Selection)
    deriving (Eq, Show)

-- | Single argument.
data Argument = Argument Name Value deriving (Eq, Show)

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
