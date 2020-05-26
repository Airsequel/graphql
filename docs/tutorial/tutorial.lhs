---
title: GraphQL Haskell Tutorial
---


== Getting started ==

Welcome to graphql-haskell!

We have written a small tutorial to help you (and ourselves) understand the graphql package.

Since this file is a literate haskell file, we start by importing some dependencies.

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import Control.Monad.IO.Class (liftIO)
> import Data.Aeson (encode)
> import Data.ByteString.Lazy.Char8 (putStrLn)
> import qualified Data.HashMap.Strict as HashMap
> import Data.Text (Text)
> import qualified Data.Text as Text
> import Data.Time (getCurrentTime)
>
> import Language.GraphQL
> import Language.GraphQL.Trans
> import Language.GraphQL.Type
> import qualified Language.GraphQL.Type.Out as Out
>
> import Prelude hiding (putStrLn)

=== First example ===

Now, as our first example, we are going to look at the
example from [graphql.js](https://github.com/graphql/graphql-js).

First we build a GraphQL schema.

> schema1 :: Schema IO
> schema1 = Schema queryType Nothing
>
> queryType :: ObjectType IO
> queryType = ObjectType "Query" Nothing []
>   $ HashMap.singleton "hello"
>   $ Field Nothing (Out.NamedScalarType string) mempty hello
>
> hello :: ActionT IO (Out.Value IO)
> hello = pure $ Out.String "it's me"

This defines a simple schema with one type and one field, that resolves to a fixed value.

Next we define our query.

> query1 :: Text
> query1 = "{ hello }"


To run the query, we call the `graphql` with the schema and the query.

> main1 :: IO ()
> main1 = putStrLn =<< encode <$> graphql schema1 query1

This runs the query by fetching the one field defined,
returning

```{"data" : {"hello":"it's me"}}```



=== Monadic actions ===

For this example, we're going to be using time.

> schema2 :: Schema IO
> schema2 = Schema queryType2 Nothing
>
> queryType2 :: ObjectType IO
> queryType2 = ObjectType "Query" Nothing []
>   $ HashMap.singleton "time"
>   $ Field Nothing (Out.NamedScalarType string) mempty time
>
> time :: ActionT IO (Out.Value IO)
> time = do
>   t <- liftIO getCurrentTime
>   pure $ Out.String $ Text.pack $ show t

This defines a simple schema with one type and one field,
which resolves to the current time.

Next we define our query.

> query2 :: Text
> query2 = "{ time }"
>
> main2 :: IO ()
> main2 = putStrLn =<< encode <$> graphql schema2 query2

This runs the query, returning the current time

```{"data": {"time":"2016-03-08 23:28:14.546899 UTC"}}```


=== Errors ===

Errors are handled according to the spec,
with fields that cause erros being resolved to `null`,
and an error being added to the error list.

An example of this is the following query:

> queryShouldFail :: Text
> queryShouldFail = "{ boyhowdy }"

Since there is no `boyhowdy` field in our schema, it will not resolve,
and the query will fail, as we can see in the following example.

> mainShouldFail :: IO ()
> mainShouldFail = do
>   success <- graphql schema1 query1
>   putStrLn $ encode success
>   putStrLn "This will fail"
>   failure <- graphql schema1 queryShouldFail
>   putStrLn $ encode failure
>

This outputs:

```
{"data": {"hello": "it's me"}}
This will fail
{"data": {"boyhowdy": null}, "errors":[{"message": "the field boyhowdy did not resolve."}]}
```

=== Combining resolvers ===

Now that we have two resolvers, we can define a schema which uses them both.

> schema3 :: Schema IO
> schema3 = Schema queryType3 Nothing
>
> queryType3 :: ObjectType IO
> queryType3 = ObjectType "Query" Nothing [] $ HashMap.fromList
>   [ ("hello", Field Nothing (Out.NamedScalarType string) mempty hello)
>   , ("time", Field Nothing (Out.NamedScalarType string) mempty time)
>   ]
>
> query3 :: Text
> query3 = "query timeAndHello { time hello }"
>
> main3 :: IO ()
> main3 = putStrLn =<< encode <$> graphql schema3 query3

This queries for both time and hello, returning

```{ "data": {"hello":"it's me","time":"2016-03-08 23:29:11.62108 UTC"}}```

Notice that we can name our queries, as we did with `timeAndHello`. Since we have only been using single queries, we can use the shorthand `{ time hello}`, as we have been doing in the previous examples.

In GraphQL there can only be one operation per query.


== Further examples ==

More examples on queries and a more complex schema can be found in the test directory,
in the [Test.StarWars](../../tests/Test/StarWars) module. This includes a more complex schema, and more complex queries.

> main :: IO ()
> main = main1 >> main2 >> mainShouldFail >> main3
