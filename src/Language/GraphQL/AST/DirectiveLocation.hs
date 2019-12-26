module Language.GraphQL.AST.DirectiveLocation
    ( DirectiveLocation
    , ExecutableDirectiveLocation
    , TypeSystemDirectiveLocation
    ) where

data DirectiveLocation
    = ExecutableDirectiveLocation ExecutableDirectiveLocation
    | TypeSystemDirectiveLocation TypeSystemDirectiveLocation
    deriving (Eq, Show)

data ExecutableDirectiveLocation
    = Query
    | Mutation
    | Subscription
    | Field
    | FragmentDefinition
    | FragmentSpread
    | InlineFragment
    deriving (Eq, Show)

data TypeSystemDirectiveLocation
    = Schema
    | Scalar
    | Object
    | FieldDefinition
    | ArgumentDefinition
    | Interface
    | Union
    | Enum
    | EnumValue
    | InputObject
    | InputFieldDefinition
    deriving (Eq, Show)
