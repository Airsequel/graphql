# GraphQL implementation in Haskell

This implementation is relatively low-level by design, it doesn't provide any
mappings between the GraphQL types and Haskell's type system and avoids
compile-time magic. It focuses on flexibility instead, so other solutions can
be built on top of it.

## State of the work

For now this library provides:

- Parser for the query and schema languages, as well as a printer for the query
  language (minimizer and pretty-printer).
- Data structures to define a type system.
- Executor (queries, mutations and subscriptions are supported).
- Validation is work in progress.
- Introspection isn't available yet.

But the idea is to be a Haskell port of
[`graphql-js`](https://github.com/graphql/graphql-js).

For a more precise list of currently missing features see
[issues](https://www.caraus.tech/projects/pub-graphql/issues).

## Documentation

API documentation is available through
[Hackage](https://hackage.haskell.org/package/graphql).

Further documentation will be made available in the
[Wiki](https://www.caraus.tech/projects/pub-graphql/wiki).

### Getting started

We start with a simple GraphQL API that provides us with some famous and less
famous cites.

```graphql
"""
Root Query type.
"""
type Query {
  """
  Provides a cite.
  """
  cite: String!
}
```

This is called a GraphQL schema, it defines all queries supported by the API.
`Query` is the root query type. Every GraphQL API should define a query type.

`Query` has a single field `cite` that returns a `String`. The `!` after the
type denotes that the returned value cannot be `Null`. GraphQL fields are
nullable by default.

To be able to work with this schema, we are going to implement it in Haskell.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.Out as Out

-- GraphQL supports 3 kinds of operations: queries, mutations and subscriptions.
-- Our first schema supports only queries.
citeSchema :: Schema IO
citeSchema = schema queryType Nothing Nothing mempty

-- GraphQL distinguishes between input and output types. Input types are field
-- argument types and they are defined in Language.GraphQL.Type.In. Output types
-- are result types, they are defined in Language.GraphQL.Type.Out. Root types
-- are always object types.
--
-- Here we define a type "Query". The second argument is an optional
-- description, the third one is the list of interfaces implemented by the
-- object type. The last argument is a field map. Keys are field names, values
-- are field definitions and resolvers. Resolvers are the functions, where the
-- actual logic lives, they return values for the respective fields.
queryType :: Out.ObjectType IO
queryType = Out.ObjectType "Query" (Just "Root Query type.") []
    $ HashMap.singleton "cite" citeResolver
  where
    -- 'ValueResolver' is a 'Resolver' data constructor, it combines a field
    -- definition with its resolver function. This function resolves a value for
    -- a field (as opposed to the 'EventStreamResolver' used by subscriptions).
    -- Our resolver just returns a constant value.
    citeResolver = ValueResolver citeField
        $ pure "Piscis primum a capite foetat"

    -- The first argument is an optional field description. The second one is
    -- the field type and the third one is for arguments (we have none in this
    -- example).
    --
    -- GraphQL has named and wrapping types. String is a scalar, named type.
    -- Named types are nullable by default. To make our "cite" field
    -- non-nullable, we wrap it in the wrapping type, Non-Null.
    citeField = Out.Field
        (Just "Provides a cite.") (Out.NonNullScalarType string) HashMap.empty

-- Now we can execute a query. Since our schema defines only one field,
-- everything we can do is to ask to resolve it and give back the result.
-- Since subscriptions don't return plain values, the 'graphql' function returns
-- an 'Either'. 'Left' is for subscriptions, 'Right' is for queries and
-- mutations.
main :: IO ()
main = do
    Right result <- graphql citeSchema "{ cite }"
    ByteString.Lazy.Char8.putStrLn $ Aeson.encode result
```

Executing this query produces the following JSON:

```json
{
  "data": {
    "cite": "Piscis primum a capite foetat"
  }
}
```

## Contact

Suggestions, patches and bug reports are welcome.

Should you have questions on usage, please open an issue and ask – this helps
to write useful documentation.
