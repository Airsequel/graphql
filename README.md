# Haskell GraphQL

[![Hackage Version](https://img.shields.io/hackage/v/graphql.svg)](https://hackage.haskell.org/package/graphql)
[![Build Status](https://semaphoreci.com/api/v1/belka-ew/graphql/branches/master/badge.svg)](https://semaphoreci.com/belka-ew/graphql)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://raw.githubusercontent.com/caraus-ecms/graphql/master/LICENSE)

For now this only provides a parser for the GraphQL query language and allows
to execute queries and mutations without the schema validation step.
But the idea is to be a Haskell port of
[`graphql-js`](https://github.com/graphql/graphql-js). Next releases should
include:

- [x] GraphQL AST
- [x] Parser for the GraphQL language.
- [x] Printer for GraphQL. This is not pretty yet.
- [ ] GraphQL Schema AST.
- [ ] Parser for the GraphQL Schema language.
- [ ] Printer for the GraphQL Schema language.
- [ ] Interpreter of GraphQL requests.
- [ ] Utilities to define GraphQL types and schema.

## Contact

Suggestions, contributions and bug reports are welcome.

Feel free to contact on Slack in [#haskell on
GraphQL](https://graphql.slack.com/messages/haskell/). You can obtain an
invitation [here](https://graphql-slack.herokuapp.com/).
