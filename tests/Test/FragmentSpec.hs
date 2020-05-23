{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.FragmentSpec
    ( spec
    ) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Language.GraphQL
import qualified Language.GraphQL.Schema as Schema
import qualified Language.GraphQL.Type as Type
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldNotSatisfy
    )
import Language.GraphQL.Type.Definition
import Language.GraphQL.Type.Schema
import Text.RawString.QQ (r)

size :: Schema.Resolver IO
size = Schema.wrappedObject "size" $ pure $ Type.S "L"

circumference :: Schema.Resolver IO
circumference = Schema.wrappedObject "circumference" $ pure $ Type.I 60

garment :: Text -> Schema.Resolver IO
garment typeName = Schema.wrappedObject "garment"
    $ pure $ Schema.object
    [ if typeName == "Hat" then circumference else size
    , Schema.wrappedObject "__typename" $ pure $ Type.S typeName
    ]

inlineQuery :: Text
inlineQuery = [r|{
  garment {
    ... on Hat {
      circumference
    }
    ... on Shirt {
      size
    }
  }
}|]

hasErrors :: Value -> Bool
hasErrors (Object object') = HashMap.member "errors" object'
hasErrors _ = True

shirtType :: ObjectType IO
shirtType = ObjectType "Shirt" Nothing
    $ HashMap.singleton resolverName
    $ Field Nothing (ScalarOutputType string) mempty resolve
  where
    (Schema.Resolver resolverName resolve) = size

hatType :: ObjectType IO
hatType = ObjectType "Hat" Nothing
    $ HashMap.singleton resolverName
    $ Field Nothing (ScalarOutputType int) mempty resolve
  where
    (Schema.Resolver resolverName resolve) = circumference

toSchema :: Schema.Resolver IO -> Schema IO
toSchema (Schema.Resolver resolverName resolve) = Schema
    { query = queryType, mutation = Nothing }
  where
    unionMember = if resolverName == "Hat" then hatType else shirtType
    queryType = ObjectType "Query" Nothing
        $ HashMap.singleton resolverName
        $ Field Nothing (ObjectOutputType unionMember) mempty resolve

spec :: Spec
spec = do
    describe "Inline fragment executor" $ do
        it "chooses the first selection if the type matches" $ do
            actual <- graphql (toSchema $ garment "Hat") inlineQuery
            let expected = object
                    [ "data" .= object
                        [ "garment" .= object
                            [ "circumference" .= (60 :: Int)
                            ]
                        ]
                    ]
             in actual `shouldBe` expected

        it "chooses the last selection if the type matches" $ do
            actual <- graphql (toSchema $ garment "Shirt") inlineQuery
            let expected = object
                    [ "data" .= object
                        [ "garment" .= object
                            [ "size" .= ("L" :: Text)
                            ]
                        ]
                    ]
             in actual `shouldBe` expected

        it "embeds inline fragments without type" $ do
            let sourceQuery = [r|{
              garment {
                circumference
                ... {
                  size
                }
              }
            }|]
                resolvers = Schema.wrappedObject "garment"
                    $ pure $ Schema.object [circumference,  size]

            actual <- graphql (toSchema resolvers) sourceQuery
            let expected = object
                    [ "data" .= object
                        [ "garment" .= object
                            [ "circumference" .= (60 :: Int)
                            , "size" .= ("L" :: Text)
                            ]
                        ]
                    ]
             in actual `shouldBe` expected

        it "evaluates fragments on Query" $ do
            let sourceQuery = [r|{
              ... {
                size
              }
            }|]

            actual <- graphql (toSchema size) sourceQuery
            actual `shouldNotSatisfy` hasErrors

    describe "Fragment spread executor" $ do
        it "evaluates fragment spreads" $ do
            let sourceQuery = [r|
              {
                ...circumferenceFragment
              }

              fragment circumferenceFragment on Hat {
                circumference
              }
            |]

            actual <- graphql (toSchema circumference) sourceQuery
            let expected = object
                    [ "data" .= object
                        [ "circumference" .= (60 :: Int)
                        ]
                    ]
             in actual `shouldBe` expected

        it "evaluates nested fragments" $ do
            let sourceQuery = [r|
              {
                garment {
                  ...circumferenceFragment
                }
              }

              fragment circumferenceFragment on Hat {
                ...hatFragment
              }

              fragment hatFragment on Hat {
                circumference
              }
            |]

            actual <- graphql (toSchema $ garment "Hat") sourceQuery
            let expected = object
                    [ "data" .= object
                        [ "garment" .= object
                            [ "circumference" .= (60 :: Int)
                            ]
                        ]
                    ]
             in actual `shouldBe` expected

        it "rejects recursive fragments" $ do
            let expected = object
                    [ "data" .= object []
                    ]
                sourceQuery = [r|
              {
                ...circumferenceFragment
              }

              fragment circumferenceFragment on Hat {
                ...circumferenceFragment
              }
            |]

            actual <- graphql (toSchema circumference) sourceQuery
            actual `shouldBe` expected

        it "considers type condition" $ do
            let sourceQuery = [r|
              {
                garment {
                  ...circumferenceFragment
                  ...sizeFragment
                }
              }
              fragment circumferenceFragment on Hat {
                circumference
              }
              fragment sizeFragment on Shirt {
                size
              }
            |]
                expected = object
                    [ "data" .= object
                        [ "garment" .= object
                            [ "circumference" .= (60 :: Int)
                            ]
                        ]
                    ]
            actual <- graphql (toSchema $ garment "Hat") sourceQuery
            actual `shouldBe` expected
