{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.DirectiveSpec
    ( spec
    ) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL
import Language.GraphQL.Type.Definition
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema (Schema(..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

experimentalResolver :: Schema IO
experimentalResolver = Schema { query = queryType, mutation = Nothing }
  where
    resolver = pure $ Int 5
    queryType = Out.ObjectType "Query" Nothing []
        $ HashMap.singleton "experimentalField"
        $ Out.Field Nothing (Out.NamedScalarType int) mempty resolver

emptyObject :: Aeson.Value
emptyObject = object
    [ "data" .= object []
    ]

spec :: Spec
spec =
    describe "Directive executor" $ do
        it "should be able to @skip fields" $ do
            let sourceQuery = [r|
              {
                experimentalField @skip(if: true)
              }
            |]

            actual <- graphql experimentalResolver sourceQuery
            actual `shouldBe` emptyObject

        it "should not skip fields if @skip is false" $ do
            let sourceQuery = [r|
              {
                experimentalField @skip(if: false)
              }
            |]
                expected = object
                    [ "data" .= object
                        [ "experimentalField" .= (5 :: Int)
                        ]
                    ]

            actual <- graphql experimentalResolver sourceQuery
            actual `shouldBe` expected

        it "should skip fields if @include is false" $ do
            let sourceQuery = [r|
              {
                experimentalField @include(if: false)
              }
            |]

            actual <- graphql experimentalResolver sourceQuery
            actual `shouldBe` emptyObject

        it "should be able to @skip a fragment spread" $ do
            let sourceQuery = [r|
              {
                ...experimentalFragment @skip(if: true)
              }

              fragment experimentalFragment on ExperimentalType {
                experimentalField
              }
            |]

            actual <- graphql experimentalResolver sourceQuery
            actual `shouldBe` emptyObject

        it "should be able to @skip an inline fragment" $ do
            let sourceQuery = [r|
              {
                ... on ExperimentalType @skip(if: true) {
                  experimentalField
                }
              }
            |]

            actual <- graphql experimentalResolver sourceQuery
            actual `shouldBe` emptyObject
