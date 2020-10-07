{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.RootOperationSpec
    ( spec
    ) where

import Data.Aeson ((.=), object)
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL
import Test.Hspec (Spec, describe, it)
import Text.RawString.QQ (r)
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.Out as Out
import Test.Hspec.GraphQL

hatType :: Out.ObjectType IO
hatType = Out.ObjectType "Hat" Nothing []
    $ HashMap.singleton "circumference"
    $ ValueResolver (Out.Field Nothing (Out.NamedScalarType int) mempty)
    $ pure $ Int 60

garmentSchema :: Schema IO
garmentSchema = schema queryType (Just mutationType) Nothing mempty
  where
    queryType = Out.ObjectType "Query" Nothing [] hatFieldResolver
    mutationType = Out.ObjectType "Mutation" Nothing [] incrementFieldResolver
    garment = pure $ Object $ HashMap.fromList
        [ ("circumference", Int 60)
        ]
    incrementFieldResolver = HashMap.singleton "incrementCircumference"
        $ ValueResolver (Out.Field Nothing (Out.NamedScalarType int) mempty)
        $ pure $ Int 61
    hatField = Out.Field Nothing (Out.NamedObjectType hatType) mempty
    hatFieldResolver =
        HashMap.singleton "garment" $ ValueResolver hatField garment

spec :: Spec
spec =
    describe "Root operation type" $ do
        it "returns objects from the root resolvers" $ do
            let querySource = [r|
              {
                garment {
                  circumference
                }
              }
            |]
                expected = HashMap.singleton "data"
                    $ object
                        [ "garment" .= object
                            [ "circumference" .= (60 :: Int)
                            ]
                        ]
            actual <- graphql garmentSchema querySource
            actual `shouldResolveTo` expected

        it "chooses Mutation" $ do
            let querySource = [r|
              mutation {
                incrementCircumference
              }
            |]
                expected = HashMap.singleton "data"
                    $ object
                        [ "incrementCircumference" .= (61 :: Int)
                        ]
            actual <- graphql garmentSchema querySource
            actual `shouldResolveTo` expected
