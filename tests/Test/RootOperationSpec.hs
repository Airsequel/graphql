{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.RootOperationSpec
    ( spec
    ) where

import Data.Aeson ((.=), object)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty(..))
import Language.GraphQL
import qualified Language.GraphQL.Schema as Schema
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)
import Language.GraphQL.Type.Definition
import Language.GraphQL.Type.Schema

hatType :: ObjectType IO
hatType = ObjectType "Hat"
    $ HashMap.singleton resolverName
    $ Field Nothing (ScalarOutputType int) mempty resolve
  where
    (Schema.Resolver resolverName resolve) =
        Schema.scalar "circumference" $ pure (60 :: Int)

schema :: Schema IO
schema = Schema
    (ObjectType "Query" hatField)
    (Just $ ObjectType "Mutation" incrementField)
  where
    queryResolvers = Schema.resolversToMap $ garment :| []
    mutationResolvers = Schema.resolversToMap $ increment :| []
    garment = Schema.object "garment" $ pure
        [ Schema.scalar "circumference" $ pure (60 :: Int)
        ]
    increment = Schema.scalar "incrementCircumference"
        $ pure (61 :: Int)
    incrementField = Field Nothing (ScalarOutputType int) mempty
        <$> mutationResolvers
    hatField = Field Nothing (ObjectOutputType hatType) mempty
        <$> queryResolvers

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
