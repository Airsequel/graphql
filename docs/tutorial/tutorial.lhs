---
title: GraphQL Haskell Tutorial
---


== Getting started ==

Welcome to GraphQL!

We have written a small tutorial to help you (and ourselves) understand the
graphql package.

Since this file is a literate haskell file, we start by importing some
dependencies.

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
> import Language.GraphQL.Type
> import qualified Language.GraphQL.Type.Out as Out
>
> import Prelude hiding (putStrLn)


=== First example ===

Now, as our first example, we are going to look at the example from
[graphql.js](https://github.com/graphql/graphql-js).

First we build a GraphQL schema.

> schema1 :: Schema IO
> schema1 = schema queryType
>
> queryType :: ObjectType IO
> queryType = ObjectType "Query" Nothing []
>   $ HashMap.singleton "hello"
>   $ ValueResolver helloField hello
>
> helloField :: Field IO
> helloField = Field Nothing (Out.NamedScalarType string) mempty
>
> hello :: Resolve IO
> hello = pure $ String "it's me"

This defines a simple schema with one type and one field, that resolves to a
fixed value.

Next we define our query.

> query1 :: Text
> query1 = "{ hello }"

To run the query, we call the `graphql` with the schema and the query.

> main1 :: IO ()
> main1 = graphql schema1 query1
>   >>= either (const $ pure ()) (putStrLn . encode)

This runs the query by fetching the one field defined, returning

```{"data" : {"hello":"it's me"}}```


=== Monadic actions ===

For this example, we're going to be using time.

> schema2 :: Schema IO
> schema2 = schema queryType2
>
> queryType2 :: ObjectType IO
> queryType2 = ObjectType "Query" Nothing []
>   $ HashMap.singleton "time"
>   $ ValueResolver timeField time
>
> timeField :: Field IO
> timeField = Field Nothing (Out.NamedScalarType string) mempty
>
> time :: Resolve IO
> time = do
>   t <- liftIO getCurrentTime
>   pure $ String $ Text.pack $ show t

This defines a simple schema with one type and one field, which resolves to the
current time.

Next we define our query.

> query2 :: Text
> query2 = "{ time }"
>
> main2 :: IO ()
> main2 = graphql schema2 query2
>   >>= either (const $ pure ()) (putStrLn . encode)

This runs the query, returning the current time

```{"data": {"time":"2016-03-08 23:28:14.546899 UTC"}}```


=== Combining resolvers ===

Now that we have two resolvers, we can define a schema which uses them both.

> schema3 :: Schema IO
> schema3 = schema queryType3
>
> queryType3 :: ObjectType IO
> queryType3 = ObjectType "Query" Nothing [] $ HashMap.fromList
>   [ ("hello", ValueResolver helloField hello)
>   , ("time", ValueResolver timeField time)
>   ]
>
> query3 :: Text
> query3 = "query timeAndHello { time hello }"
>
> main3 :: IO ()
> main3 = graphql schema3 query3
>   >>= either (const $ pure ()) (putStrLn . encode)

This queries for both time and hello, returning

```{ "data": {"hello":"it's me","time":"2016-03-08 23:29:11.62108 UTC"}}```

Notice that we can name our queries, as we did with `timeAndHello`. Since we
have only been using single queries, we can use the shorthand `{ time hello }`,
as we have been doing in the previous examples.

In GraphQL there can only be one operation per query.


== Further examples ==

More examples on queries and a more complex schema can be found in the test
directory, in the [Test.StarWars](../../tests/Test/StarWars) module. This
includes a more complex schema, and more complex queries.

> main :: IO ()
> main = main1 >> main2 >> main3
