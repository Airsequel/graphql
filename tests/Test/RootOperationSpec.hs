{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.RootOperationSpec
    ( spec
    ) where

import Data.Aeson ((.=), object)
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL
import qualified Language.GraphQL.Schema as Schema
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)
import Language.GraphQL.Type.Definition
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema

hatType :: Out.ObjectType IO
hatType = Out.ObjectType "Hat" Nothing []
    $ HashMap.singleton resolverName
    $ Out.Field Nothing (Out.NamedScalarType int) mempty resolve
  where
    (Schema.Resolver resolverName resolve) =
        Schema.Resolver "circumference" $ pure $ Out.Int 60

schema :: Schema IO
schema = Schema
    (Out.ObjectType "Query" Nothing [] hatField)
    (Just $ Out.ObjectType "Mutation" Nothing [] incrementField)
  where
    garment = pure $ Schema.object
        [ Schema.Resolver "circumference" $ pure $ Out.Int 60
        ]
    incrementField = HashMap.singleton "incrementCircumference"
        $ Out.Field Nothing (Out.NamedScalarType int) mempty
        $ pure $ Out.Int 61
    hatField = HashMap.singleton "garment"
        $ Out.Field Nothing (Out.NamedObjectType hatType) mempty garment

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
