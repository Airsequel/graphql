{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.DirectiveSpec
    ( spec
    ) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.Out as Out
import Test.Hspec (Spec, describe, it)
import Test.Hspec.GraphQL
import Text.RawString.QQ (r)

experimentalResolver :: Schema IO
experimentalResolver = Schema
    { query = queryType, mutation = Nothing, subscription = Nothing }
  where
    queryType = Out.ObjectType "Query" Nothing []
        $ HashMap.singleton "experimentalField"
        $ Out.ValueResolver (Out.Field Nothing (Out.NamedScalarType int) mempty)
        $ pure $ Int 5

emptyObject :: Aeson.Object
emptyObject = HashMap.singleton "data" $ object []

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
            actual `shouldResolveTo` emptyObject

        it "should not skip fields if @skip is false" $ do
            let sourceQuery = [r|
              {
                experimentalField @skip(if: false)
              }
            |]
                expected = HashMap.singleton "data"
                    $ object
                        [ "experimentalField" .= (5 :: Int)
                        ]
            actual <- graphql experimentalResolver sourceQuery
            actual `shouldResolveTo` expected

        it "should skip fields if @include is false" $ do
            let sourceQuery = [r|
              {
                experimentalField @include(if: false)
              }
            |]

            actual <- graphql experimentalResolver sourceQuery
            actual `shouldResolveTo` emptyObject

        it "should be able to @skip a fragment spread" $ do
            let sourceQuery = [r|
              {
                ...experimentalFragment @skip(if: true)
              }

              fragment experimentalFragment on Query {
                experimentalField
              }
            |]

            actual <- graphql experimentalResolver sourceQuery
            actual `shouldResolveTo` emptyObject

        it "should be able to @skip an inline fragment" $ do
            let sourceQuery = [r|
              {
                ... on Query @skip(if: true) {
                  experimentalField
                }
              }
            |]

            actual <- graphql experimentalResolver sourceQuery
            actual `shouldResolveTo` emptyObject
