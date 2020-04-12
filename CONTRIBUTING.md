# Contributing guidelines

## Testing

To ensure all code changes adhere to existing code quality standards, some
automatic checks can be run locally.

Ensure that the code builds without warnings and passes the tests:

```sh
stack test --pedantic
```

And also run the linter on your code:

```sh
stack build hlint
stack exec hlint -- src tests
```

Build the documentation and check if you get any warnings:

```sh
stack haddock
```

Validate that literate Haskell (tutorials) files compile without any warnings:

```sh
stack ghc -- -Wall -fno-code docs/tutorial/*.lhs
```
