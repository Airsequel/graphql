# GraphQL implementation in Haskell

[![Hackage Version](https://img.shields.io/hackage/v/graphql.svg)](https://hackage.haskell.org/package/graphql)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/graphql/badge)](https://matrix.hackage.haskell.org/package/graphql)
[![Build Status](https://github.com/caraus-ecms/graphql/workflows/Haskell%20CI/badge.svg)](https://github.com/caraus-ecms/graphql/actions?query=workflow%3A%22Haskell+CI%22)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://raw.githubusercontent.com/caraus-ecms/graphql/master/LICENSE)
[![Simple Haskell](https://www.simplehaskell.org/badges/badge.svg)](https://www.simplehaskell.org)

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

For a more precise list of currently missing features see issues marked as
"[not implemented](https://github.com/caraus-ecms/graphql/labels/not%20implemented)".

## Documentation

API documentation is available through
[Hackage](https://hackage.haskell.org/package/graphql).

You'll also find a small tutorial with some examples under
[docs/tutorial](https://github.com/caraus-ecms/graphql/tree/master/docs/tutorial).

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

import Control.Exception (SomeException)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.Out as Out

-- GraphQL supports 3 kinds of operations: queries, mutations and subscriptions.
-- Our first schema supports only queries.
schema :: Schema IO
schema = Schema
    { query = queryType, mutation = Nothing, subscription = Nothing }

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
    Right result <- graphql schema "{ cite }"
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

## Further information

- [Contributing guidelines](CONTRIBUTING.md).
- [Changelog](CHANGELOG.md) – this one contains the most recent changes; 
  individual changelogs for specific versions can be found on
  [Hackage](https://hackage.haskell.org/package/graphql).

## Contact

Suggestions, contributions and bug reports are welcome.

Should you have questions on usage, please open an issue and ask – this helps
to write useful documentation.

Feel free to contact on Slack in [#haskell on
GraphQL](https://graphql.slack.com/messages/haskell/). You can obtain an
invitation [here](https://graphql-slack.herokuapp.com/).
