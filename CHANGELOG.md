# Change Log
All notable changes to this project will be documented in this file.

## [Unreleased]
### Added
- Minimal documentation for all public symbols.

### Deprecated
- Language.GraphQL.AST.FragmentName. Replaced with Language.GraphQL.AST.Name.

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

[0.5.0.0]: https://github.com/caraus-ecms/graphql/compare/v0.4.0.0...v0.5.0.0
[0.4.0.0]: https://github.com/caraus-ecms/graphql/compare/v0.3...v0.4.0.0
[0.3]: https://github.com/caraus-ecms/graphql/compare/v0.2.1...v0.3
[0.2.1]: https://github.com/caraus-ecms/graphql/compare/v0.2...v0.2.1
[0.2]: https://github.com/caraus-ecms/graphql/compare/v0.1...v0.2
