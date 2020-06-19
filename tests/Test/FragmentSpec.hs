{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.FragmentSpec
    ( spec
    ) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Language.GraphQL
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.Out as Out
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldNotSatisfy
    )
import Text.RawString.QQ (r)

size :: (Text, Value)
size = ("size", String "L")

circumference :: (Text, Value)
circumference = ("circumference", Int 60)

garment :: Text -> (Text, Value)
garment typeName =
    ("garment",  Object $ HashMap.fromList
        [ if typeName == "Hat" then circumference else size
        , ("__typename", String typeName)
        ]
    )

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

hasErrors :: Aeson.Value -> Bool
hasErrors (Aeson.Object object') = HashMap.member "errors" object'
hasErrors _ = True

shirtType :: Out.ObjectType IO
shirtType = Out.ObjectType "Shirt" Nothing []
    $ HashMap.fromList
        [ ("size", Out.Resolver sizeFieldType $ pure $ snd size)
        , ("circumference", Out.Resolver circumferenceFieldType $ pure $ snd circumference)
        ]

hatType :: Out.ObjectType IO
hatType = Out.ObjectType "Hat" Nothing []
    $ HashMap.fromList
        [ ("size", Out.Resolver sizeFieldType $ pure $ snd size)
        , ("circumference", Out.Resolver circumferenceFieldType $ pure $ snd circumference)
        ]

circumferenceFieldType :: Out.Field IO
circumferenceFieldType = Out.Field Nothing (Out.NamedScalarType int) mempty

sizeFieldType :: Out.Field IO
sizeFieldType = Out.Field Nothing (Out.NamedScalarType string) mempty

toSchema :: Text -> (Text, Value) -> Schema IO
toSchema t (_, resolve) = Schema
    { query = queryType, mutation = Nothing }
  where
    unionMember = if t == "Hat" then hatType else shirtType
    typeNameField = Out.Field Nothing (Out.NamedScalarType string) mempty
    garmentField = Out.Field Nothing (Out.NamedObjectType unionMember) mempty
    queryType =
        case t of
            "circumference" -> hatType
            "size" -> shirtType
            _ -> Out.ObjectType "Query" Nothing []
                $ HashMap.fromList
                    [ ("garment", Out.Resolver garmentField $ pure resolve)
                    , ("__typename", Out.Resolver typeNameField $ pure $ String "Shirt")
                    ]

spec :: Spec
spec = do
    describe "Inline fragment executor" $ do
        it "chooses the first selection if the type matches" $ do
            actual <- graphql (toSchema "Hat" $ garment "Hat") inlineQuery
            let expected = object
                    [ "data" .= object
                        [ "garment" .= object
                            [ "circumference" .= (60 :: Int)
                            ]
                        ]
                    ]
             in actual `shouldBe` expected

        it "chooses the last selection if the type matches" $ do
            actual <- graphql (toSchema "Shirt" $ garment "Shirt") inlineQuery
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
                resolvers = ("garment", Object $ HashMap.fromList [circumference,  size])

            actual <- graphql (toSchema "garment" resolvers) sourceQuery
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

            actual <- graphql (toSchema "size" size) sourceQuery
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

            actual <- graphql (toSchema "circumference" circumference) sourceQuery
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

            actual <- graphql (toSchema "Hat" $ garment "Hat") sourceQuery
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

            actual <- graphql (toSchema "circumference" circumference) sourceQuery
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
            actual <- graphql (toSchema "Hat" $ garment "Hat") sourceQuery
            actual `shouldBe` expected
