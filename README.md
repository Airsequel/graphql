# GraphQL implementation in Haskell

[![Simple Haskell](https://www.simplehaskell.org/badges/badge.svg)](https://www.simplehaskell.org)
[![CI/CD](https://img.shields.io/badge/CI-CD-brightgreen)](https://build.caraus.tech/go/pipelines)

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
