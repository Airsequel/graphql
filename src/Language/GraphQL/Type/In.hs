-- | This module is intended to be imported qualified, to avoid name clashes
-- with 'Language.GraphQL.Type.Out'.
module Language.GraphQL.Type.In
    ( Value(..)
    ) where

import Data.HashMap.Strict (HashMap)
import Data.Int (Int32)
import Data.String (IsString(..))
import Data.Text (Text)
import Language.GraphQL.AST.Document (Name)

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
