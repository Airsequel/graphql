{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.RootOperationSpec
    ( spec
    ) where

import Data.Aeson ((.=), object)
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.Out as Out

hatType :: Out.ObjectType IO
hatType = Out.ObjectType "Hat" Nothing []
    $ HashMap.singleton "circumference"
    $ ValueResolver (Out.Field Nothing (Out.NamedScalarType int) mempty)
    $ pure $ Int 60

schema :: Schema IO
schema = Schema
    (Out.ObjectType "Query" Nothing [] hatFieldResolver)
    (Just $ Out.ObjectType "Mutation" Nothing [] incrementFieldResolver)
  where
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
                expected = object
                    [ "data" .= object
                        [ "garment" .= object
                            [ "circumference" .= (60 :: Int)
                            ]
                        ]
                    ]
            actual <- graphql schema querySource
            actual `shouldBe` expected

        it "chooses Mutation" $ do
            let querySource = [r|
              mutation {
                incrementCircumference
              }
            |]
                expected = object
                    [ "data" .= object
                        [ "incrementCircumference" .= (61 :: Int)
                        ]
                    ]
            actual <- graphql schema querySource
            actual `shouldBe` expected
