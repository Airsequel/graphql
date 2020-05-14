{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.RootOperationSpec
    ( spec
    ) where

import Data.Aeson ((.=), object)
import Data.List.NonEmpty (NonEmpty(..))
import Language.GraphQL
import qualified Language.GraphQL.Schema as Schema
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)
import Language.GraphQL.Type.Definition
import Language.GraphQL.Type.Schema

schema :: Schema IO
schema = Schema
    (ObjectType "Query" queryResolvers)
    (Just $ ObjectType "Mutation" mutationResolvers)
  where
    queryResolvers = Schema.resolversToMap $ garment :| []
    mutationResolvers = Schema.resolversToMap $ increment :| []
    garment = Schema.object "garment" $ pure
        [ Schema.scalar "circumference" $ pure (60 :: Int)
        ]
    increment = Schema.scalar "incrementCircumference"
        $ pure (61 :: Int)

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
