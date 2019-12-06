{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.DirectiveSpec
    ( spec
    ) where

import Data.Aeson (Value, object, (.=))
import Data.List.NonEmpty (NonEmpty(..))
import Language.GraphQL
import qualified Language.GraphQL.Schema as Schema
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

experimentalResolver :: Schema.Resolver IO
experimentalResolver = Schema.scalar "experimentalField" $ pure (5 :: Int) 

emptyObject :: Value
emptyObject = object
    [ "data" .= object []
    ]

spec :: Spec
spec =
    describe "Directive executor" $ do
        it "should be able to @skip fields" $ do
            let query = [r|
              {
                experimentalField @skip(if: true)
              }
            |]

            actual <- graphql (experimentalResolver :| []) query
            actual `shouldBe` emptyObject

        it "should not skip fields if @skip is false" $ do
            let query = [r|
              {
                experimentalField @skip(if: false)
              }
            |]
                expected = object
                    [ "data" .= object
                        [ "experimentalField" .= (5 :: Int)
                        ]
                    ]

            actual <- graphql (experimentalResolver :| []) query
            actual `shouldBe` expected

        it "should skip fields if @include is false" $ do
            let query = [r|
              {
                experimentalField @include(if: false)
              }
            |]

            actual <- graphql (experimentalResolver :| []) query
            actual `shouldBe` emptyObject

        it "should be able to @skip a fragment spread" $ do
            let query = [r|
              {
                ...experimentalFragment @skip(if: true)
              }

              fragment experimentalFragment on ExperimentalType {
                experimentalField
              }
            |]

            actual <- graphql (experimentalResolver :| []) query
            actual `shouldBe` emptyObject

        it "should be able to @skip an inline fragment" $ do
            let query = [r|
              {
                ... on ExperimentalType @skip(if: true) {
                  experimentalField
                }
              }
            |]

            actual <- graphql (experimentalResolver :| []) query
            actual `shouldBe` emptyObject
