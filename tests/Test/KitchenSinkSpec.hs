{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.KitchenSinkSpec
    ( spec
    ) where

import qualified Data.Text.IO as Text.IO
import qualified Language.GraphQL.Encoder as Encoder
import qualified Language.GraphQL.Parser as Parser
import Paths_graphql (getDataFileName)
import Test.Hspec ( Spec
                  , describe
                  , it
                  )
import Test.Hspec.Expectations ( expectationFailure
                               , shouldBe
                               )
import Text.Megaparsec ( errorBundlePretty
                       , parse
                       )
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Kitchen Sink" $ do
    it "minifies the query" $ do
        dataFileName <- getDataFileName "tests/data/kitchen-sink.graphql"
        minFileName <- getDataFileName "tests/data/kitchen-sink.min.graphql"
        actual <- Text.IO.readFile dataFileName
        expected <- Text.IO.readFile minFileName

        either
            (expectationFailure . errorBundlePretty)
            (flip shouldBe expected . Encoder.document Encoder.minified)
            $ parse Parser.document dataFileName actual

    it "pretty prints the query" $ do
        dataFileName <- getDataFileName "tests/data/kitchen-sink.graphql"
        actual <- Text.IO.readFile dataFileName
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

        either
            (expectationFailure . errorBundlePretty)
            (flip shouldBe expected . Encoder.document Encoder.pretty)
            $ parse Parser.document dataFileName actual
