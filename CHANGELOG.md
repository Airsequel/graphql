# Changelog
All notable changes to this project will be documented in this file.

The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]
### Fixed
- The parser rejects variables when parsing defaultValue (DefaultValue). The
  specification defines default values as `Value` with `const` parameter and
  constants cannot be variables. `AST.Document.ConstValue` was added,
  `AST.Document.ObjectField` was modified.
- AST transformation should never fail.
    * Missing variable are assumed to be null.
    * Invalid (recusrive or non-existing) fragments should be skipped.

### Changed
- `Schema.Resolver` cannot return arbitrary JSON anymore, but only
  `Type.Definition.Value`.
- `AST.Core.Value` was moved into `Type.Definition`. These values are used only
  in the execution and type system, it is not a part of the parsing tree.
- `Type` module is superseded by `Type.Out`. This module contains now only
  exports from other module that complete `Type.In` and `Type.Out` exports.
- `Error.CollectErrsT` contains the new `Resolution` data structure.
  `Resolution` represents the state used by the executor. It contains all types
  defined in the schema and collects the thrown errors.

### Added
- `Type.Definition` contains base type system definition, e.g. Enums and
  Scalars.
- `Type.Schema` describes a schema. Both public functions that execute queries
  accept a `Schema` now instead of a `HashMap`. The execution fails if the root
  operation doesn't match the root Query type in the schema.
- `Type.In` and `Type.Out` contain definitions for input and output types.
- `Execute.Coerce` defines a typeclass responsible for input, variable value
  coercion. It decouples us a bit from JSON since any format can be used to pass
  query variables. Execution functions accept (`HashMap Name a`) instead of
  `Subs`, where a is an instance of `VariableValue`.

### Removed
- `Schema.scalar`, `Schema.wrappedScalar`. They accepted everything can be
  converted to JSON and JSON is not suitable as an internal representation for
  GraphQL. E.g. GraphQL distinguishes between Floats and Integers.
- `Schema.wrappedObject`, `Schema.object`, `Schema.resolversToMap`. There is no
  need in special functions to construct field resolvers anymore, resolvers are
  normal functions attached to the fields in the schema representation.
- `Error.runAppendErrs` isn't used anywhere.
- `AST.Core`: `Document`, `Directive`, `Field`, `Fragment`, `Selection`, `Alias`
  `TypeCondition` were modified, moved into `Execute.Transform.Document` and
  made private. These types describe intermediate representation used by the
  executor internally. Moving was required to avoid cyclic dependencies between
  the executor and type system.

## [0.7.0.0] - 2020-05-11
### Fixed
- Result of null encoding
- Block strings encoding
- Result of tab and newline encoding

### Added
- AST for the GraphQL schema.
- Type system definition parser.
- `Trans.argument`.
- Schema extension parser.
- Contributing guidelines.
- `Schema.resolversToMap` (intended to be used internally).

### Changed
- Rename `AST.Definition` into `AST.Document.ExecutableDefinition`.
  `AST.Document.TypeSystemDefinition` and `AST.Document.TypeSystemExtension`
  can also be definitions.
- Move all AST data to `AST.Document` and reexport them.
- Rename `AST.OperationSelectionSet` to `AST.Document.SelectionSet`.
- Make `Schema.Subs` a `Data.HashMap.Strict` (was a function
  `key -> Maybe value` before).
- Make `AST.Lexer.at` a text (symbol) parser. It was a char before and is
  `symbol "@"` now.
- Replace `MonadIO` with a plain `Monad`. Since the tests don't use IO,
  set the inner monad to `Identity`.
- `NonEmpty (Resolver m)` is now `HashMap Text (NonEmpty (Resolver m))`. Root
  operation type can be any type, therefore a hashmap is needed. Since types
  cannot be empty, we save the list of resolvers in the type as a non-empty
  list. Currently only "Query" and "Mutation" are supported as types. For more
  schema support is required. The executor checks now if the type in the query
  matches the type of the provided root resolvers.

### Removed
- `AST.Field`, `AST.InlineFragment` and `AST.FragmentSpread`.
  These types are only used in `AST.Selection` and `AST.Selection` contains now
  3 corresponding data constructors, `Field`, `InlineFragment` and
  `FragmentSpread`, instead of separate types. It simplifies pattern matching
  and doesn't make the code less typesafe.
- `Schema.scalarA`.
- `Schema.wrappedScalarA`.
- `Schema.wrappedObjectA`.
- `Schema.objectA`.
- `AST.Argument`. Replaced with `AST.Arguments` which holds all arguments as a
  key/value map.

## [0.6.1.0] - 2019-12-23
### Fixed
- Parsing multiple string arguments, such as
  `login(username: "username", password: "password")` would fail on the comma
  due to strings not having a space consumer.
- Fragment spread is evaluated based on the `__typename` resolver. If the
  resolver is missing, it is assumed that the type condition is satisfied (all
  fragments are included).
- Escaping characters during encoding.

### Added
- Directive support (@skip and @include).
- Pretifying multi-line string arguments as block strings.

## [0.6.0.0] - 2019-11-27
### Changed
- `Language.GraphQL.Encoder` moved to `Language.GraphQL.AST.Encoder`.
- `Language.GraphQL.Parser` moved to `Language.GraphQL.AST.Parser`.
- `Language.GraphQL.Lexer` moved to `Language.GraphQL.AST.Lexer`.
- All `Language.GraphQL.AST.Value` data constructor prefixes were removed. The
  module should be imported qualified.
- All `Language.GraphQL.AST.Core.Value` data constructor prefixes were removed.
  The module should be imported qualified.
- `Language.GraphQL.AST.Core.Object` is now just a HashMap.
- `Language.GraphQL.AST.Transform` is isn't exposed publically anymore.
- `Language.GraphQL.Schema.resolve` accepts a selection `Seq` (`Data.Sequence`)
  instead of a list. Selections are stored as sequences internally as well.
- Add a reader instance to the resolver's monad stack. The Reader contains
  a Name/Value hashmap, which will contain resolver arguments.

### Added
- Nested fragment support.

### Fixed
- Consume ignored tokens after `$` and `!`. I mistakenly assumed that
  `$variable` is a single token, same as `Type!` is a single token. This is not
  the case, for example `Variable` is defined as `$ Name`, so these are two
  tokens, therefore whitespaces and commas after `$` and `!` should be
  consumed.

### Improved
- `Language.GraphQL.AST.Parser.type_`: Try type parsers in a variable
  definition in a different order to avoid using `but`.

### Removed
- `Language.GraphQL.AST.Arguments`. Use `[Language.GraphQL.AST.Argument]`
  instead.
- `Language.GraphQL.AST.Directives`. Use `[Language.GraphQL.AST.Directives]`
  instead.
- `Language.GraphQL.AST.VariableDefinitions`. Use
  `[Language.GraphQL.AST.VariableDefinition]` instead.
- `Language.GraphQL.AST.FragmentName`. Use `Language.GraphQL.AST.Name` instead.
- `Language.GraphQL.Execute.Schema` - It was a resolver list, not a schema.
- `Language.GraphQL.Schema`: `enum`, `enumA`, `wrappedEnum` and `wrappedEnumA`.
  Use `scalar`, `scalarA`, `wrappedScalar` and `wrappedScalarA` instead.


## [0.5.1.0] - 2019-10-22
### Deprecated
- `Language.GraphQL.AST.Arguments`. Use `[Language.GraphQL.AST.Argument]`
  instead.
- `Language.GraphQL.AST.Directives`. Use `[Language.GraphQL.AST.Directives]`
  instead.
- `Language.GraphQL.AST.VariableDefinitions`. Use
  `[Language.GraphQL.AST.VariableDefinition]` instead.

### Added
- Module documentation.
- Inline fragment support.

### Fixed
- Top-level fragments.
- Fragment for execution is chosen based on the type.

## [0.5.0.1] - 2019-09-10
### Added
- Minimal documentation for all public symbols.

### Deprecated
- `Language.GraphQL.AST.FragmentName`. Replaced with Language.GraphQL.AST.Name.
- `Language.GraphQL.Execute.Schema` - It is not a schema (at least not a
  complete one), but a resolver list, and the resolvers should be provided by
  the user separately, because the schema can originate from a GraphQL
  document. `Schema` name should be free to provide a data type for the real
  schema later.
- `Language.GraphQL.Schema`: `enum`, `enumA`, `wrappedEnum` and `wrappedEnumA`.
  There are actually only two generic types in GraphQL: Scalars and objects.
  Enum is a scalar value.

### Fixed
- Parsing block string values.

## [0.5.0.0] - 2019-08-14
### Added
- `executeWithName` executes an operation with the given name.
- Export `Language.GraphQL.Encoder.definition`,
  `Language.GraphQL.Encoder.type'` and `Language.GraphQL.Encoder.directive`.
- Export `Language.GraphQL.Encoder.value`. Escapes \ and " in strings now.

### Changed
- `Operation` includes now possible operation name which allows to support
  documents with multiple operations.
- `Language.GraphQL.Encoder.document` and other encoding functions take a
  `Formatter` as argument to distinguish between minified and pretty printing.
- All encoder functions return `Data.Text.Lazy`.

### Removed
- Unused `Language.GraphQL.Encoder.spaced`.

## [0.4.0.0] - 2019-07-23
### Added
- Support for mutations.
- Error handling (with monad transformers).
- Nullable types.
- Arbitrary nested lists support.
- Potential BOM header parsing.

### Changed
- attoparsec is replaced with megaparsec.
- The library is now under `Language.GraphQL` (instead of `Data.GraphQL`).
- HUnit and tasty are replaced with Hspec.
- `Alternative`/`MonadPlus` resolver constraints are replaced with `MonadIO`.

### Removed
- Duplicates from `Language.GraphQL.AST` already available in
  `Language.GraphQL.AST.Core`.
- All module exports are now explicit, so private and help functions aren't
  exported anymore.

## [0.3] - 2015-09-22
### Changed
- Exact match numeric types to spec.
- Names follow now the spec.
- AST slightly different for better readability or easier parsing.
- Replace golden test for test to validate parsing/encoding.

### Added
- Parsing errors in all cases where `Alternative` is used.
- GraphQL encoder.

### Fixed
- Expect braces `inputValueDefinitions` instead of parens when parsing.

## [0.2.1] - 2015-09-16
### Fixed
- Include data files for golden tests in Cabal package.
- Support for ghc-7.8.

## [0.2] - 2015-09-14
### Added
- Rudimentary parser for `GraphQL` which successfully parses the sample file
  `kitchen-sink.graphql` from `graphql-js` tests.
- Golden test for `kitchen-sink.grahql` parsing.
### Changed
- Many optional data types in `GraphQl` didn't need to be wrapped in a `Maybe`.
- Some `newtype`s became type synonyms for easier parsing.

## 0.1 - 2015-09-12
### Added
- Data types for the GraphQL language.

[Unreleased]: https://github.com/caraus-ecms/graphql/compare/v0.6.1.0...HEAD
[0.7.0.0]: https://github.com/caraus-ecms/graphql/compare/v0.6.1.0...v0.7.0.0
[0.6.1.0]: https://github.com/caraus-ecms/graphql/compare/v0.6.0.0...v0.6.1.0
[0.6.0.0]: https://github.com/caraus-ecms/graphql/compare/v0.5.1.0...v0.6.0.0
[0.5.1.0]: https://github.com/caraus-ecms/graphql/compare/v0.5.0.1...v0.5.1.0
[0.5.0.1]: https://github.com/caraus-ecms/graphql/compare/v0.5.0.0...v0.5.0.1
[0.5.0.0]: https://github.com/caraus-ecms/graphql/compare/v0.4.0.0...v0.5.0.0
[0.4.0.0]: https://github.com/caraus-ecms/graphql/compare/v0.3...v0.4.0.0
[0.3]: https://github.com/caraus-ecms/graphql/compare/v0.2.1...v0.3
[0.2.1]: https://github.com/caraus-ecms/graphql/compare/v0.2...v0.2.1
[0.2]: https://github.com/caraus-ecms/graphql/compare/v0.1...v0.2
