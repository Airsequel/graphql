# Haskell GraphQL

[![Hackage Version](https://img.shields.io/hackage/v/graphql.svg)](https://hackage.haskell.org/package/graphql)
[![Build Status](https://semaphoreci.com/api/v1/belka-ew/graphql/branches/master/badge.svg)](https://semaphoreci.com/belka-ew/graphql)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://raw.githubusercontent.com/caraus-ecms/graphql/master/LICENSE)

GraphQL implementation in Haskell.

This implementation is relatively low-level by design, it doesn't provide any
mappings between the GraphQL types and Haskell's type system and avoids
compile-time magic. It focuses on flexibility instead instead, so other
solutions can be built on top of it.

## State of the work

For now this only provides a parser and a printer for the GraphQL query
language and allows to execute queries and mutations without the schema
validation step. But the idea is to be a Haskell port of
[`graphql-js`](https://github.com/graphql/graphql-js).

For the list of currently missing features see issues marked as
"[not implemented](https://github.com/caraus-ecms/graphql/labels/not%20implemented)".

## Documentation

API documentation is available through
[hackage](https://hackage.haskell.org/package/graphql).

You'll also find a small tutorial with some examples under
[docs/tutorial](https://github.com/caraus-ecms/graphql/tree/master/docs/tutorial).

## Contact

Suggestions, contributions and bug reports are welcome.

Should you have questions on usage, please open an issue and ask – this helps
to write useful documentation.

Feel free to contact on Slack in [#haskell on
GraphQL](https://graphql.slack.com/messages/haskell/). You can obtain an
invitation [here](https://graphql-slack.herokuapp.com/).
