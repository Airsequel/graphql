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
import Language.GraphQL.Type.Schema
import qualified Language.GraphQL.Type as Type

hatType :: ObjectType IO
hatType = ObjectType "Hat" Nothing
    $ HashMap.singleton resolverName
    $ Field Nothing (ScalarOutputType int) mempty resolve
  where
    (Schema.Resolver resolverName resolve) =
        Schema.wrappedObject "circumference" $ pure $ Type.I 60

schema :: Schema IO
schema = Schema
    (ObjectType "Query" Nothing hatField)
    (Just $ ObjectType "Mutation" Nothing incrementField)
  where
    garment = NestingResolver
        $ pure $ Schema.object
        [ Schema.wrappedObject "circumference" $ pure $ Type.I 60
        ]
    incrementField = HashMap.singleton "incrementCircumference"
        $ Field Nothing (ScalarOutputType int) mempty
        $ NestingResolver $ pure $ Type.I 61
    hatField = HashMap.singleton "garment"
        $ Field Nothing (ObjectOutputType hatType) mempty garment

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
