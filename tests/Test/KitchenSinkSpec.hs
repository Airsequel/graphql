{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.KitchenSinkSpec
    ( spec
    ) where

import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Language.GraphQL.AST.Encoder as Encoder
import qualified Language.GraphQL.AST.Parser as Parser
import Paths_graphql (getDataFileName)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (parseSatisfies)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Kitchen Sink" $ do
    it "minifies the query" $ do
        dataFileName <- getDataFileName "tests/data/kitchen-sink.graphql"
        minFileName <- getDataFileName "tests/data/kitchen-sink.min.graphql"
        expected <- Text.Lazy.IO.readFile minFileName

        shouldNormalize Encoder.minified dataFileName expected

    it "pretty prints the query" $ do
        dataFileName <- getDataFileName "tests/data/kitchen-sink.graphql"
        let expected = [r|query queryName($foo: ComplexType, $site: Site = MOBILE) {
  whoever123is: node(id: [123, 456]) {
    id
    ... on User @defer {
      field2 {
        id
        alias: field1(first: 10, after: $foo) @include(if: $foo) {
          id
          ...frag
        }
      }
    }
  }
}

mutation likeStory {
  like(story: 123) @defer {
    story {
      id
    }
  }
}

fragment frag on Friend {
  foo(size: $size, bar: $b, obj: {key: "value"})
}

{
  unnamed(truthy: true, falsey: false)
  query
}
|]

        shouldNormalize Encoder.pretty dataFileName expected

shouldNormalize :: Encoder.Formatter -> FilePath -> Lazy.Text -> IO ()
shouldNormalize formatter dataFileName expected = do
    actual <- Text.IO.readFile dataFileName
    parse Parser.document dataFileName actual `parseSatisfies` condition
      where
        condition = (expected ==) . Encoder.document formatter
