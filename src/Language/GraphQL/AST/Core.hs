-- | This is the AST meant to be executed.
module Language.GraphQL.AST.Core
    ( Argument(..)
    , Document
    , Field(..)
    , Name
    , ObjectField(..)
    , Operation(..)
    , Value(..)
    ) where

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.String

import Data.Text (Text)

type Name = Text

type Document = NonEmpty Operation

data Operation = Query    (NonEmpty Field)
               | Mutation (NonEmpty Field)
                 deriving (Eq,Show)

data Field = Field (Maybe Alias) Name [Argument] [Field] deriving (Eq,Show)

type Alias = Name

data Argument = Argument Name Value deriving (Eq,Show)

data Value = ValueInt Int32
           -- GraphQL Float is double precision
           | ValueFloat Double
           | ValueString Text
           | ValueBoolean Bool
           | ValueNull
           | ValueEnum Name
           | ValueList [Value]
           | ValueObject [ObjectField]
             deriving (Eq,Show)

instance IsString Value where
  fromString = ValueString . fromString

data ObjectField = ObjectField Name Value deriving (Eq,Show)
