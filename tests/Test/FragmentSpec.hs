{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.FragmentSpec
    ( spec
    ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Language.GraphQL
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.Out as Out
import Test.Hspec (Spec, describe, it)
import Test.Hspec.GraphQL
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

shirtType :: Out.ObjectType IO
shirtType = Out.ObjectType "Shirt" Nothing [] $ HashMap.fromList
    [ ("size", sizeFieldType)
    ]

hatType :: Out.ObjectType IO
hatType = Out.ObjectType "Hat" Nothing [] $ HashMap.fromList
    [ ("size", sizeFieldType)
    , ("circumference", circumferenceFieldType)
    ]

circumferenceFieldType :: Out.Resolver IO
circumferenceFieldType
    = Out.ValueResolver (Out.Field Nothing (Out.NamedScalarType int) mempty)
    $ pure $ snd circumference

sizeFieldType :: Out.Resolver IO
sizeFieldType
    = Out.ValueResolver (Out.Field Nothing (Out.NamedScalarType string) mempty)
    $ pure $ snd size

toSchema :: Text -> (Text, Value) -> Schema IO
toSchema t (_, resolve) = Schema
    { query = queryType, mutation = Nothing, subscription = Nothing }
  where
    garmentType = Out.UnionType "Garment" Nothing [hatType, shirtType]
    typeNameField = Out.Field Nothing (Out.NamedScalarType string) mempty
    garmentField = Out.Field Nothing (Out.NamedUnionType garmentType) mempty
    queryType =
        case t of
            "circumference" -> hatType
            "size" -> shirtType
            _ -> Out.ObjectType "Query" Nothing []
                $ HashMap.fromList
                    [ ("garment", ValueResolver garmentField (pure resolve))
                    , ("__typename", ValueResolver typeNameField (pure $ String "Shirt"))
                    ]

spec :: Spec
spec = do
    describe "Inline fragment executor" $ do
        it "chooses the first selection if the type matches" $ do
            actual <- graphql (toSchema "Hat" $ garment "Hat") inlineQuery
            let expected = HashMap.singleton "data"
                    $ Aeson.object
                        [ "garment" .= Aeson.object
                            [ "circumference" .= (60 :: Int)
                            ]
                        ]
             in actual `shouldResolveTo` expected

        it "chooses the last selection if the type matches" $ do
            actual <- graphql (toSchema "Shirt" $ garment "Shirt") inlineQuery
            let expected = HashMap.singleton "data"
                    $ Aeson.object
                        [ "garment" .= Aeson.object
                            [ "size" .= ("L" :: Text)
                            ]
                        ]
             in actual `shouldResolveTo` expected

        it "embeds inline fragments without type" $ do
            let sourceQuery = [r|{
              garment {
                circumference
                ... {
                  size
                }
              }
            }|]
            actual <- graphql (toSchema "garment" $ garment "Hat") sourceQuery
            let expected = HashMap.singleton "data"
                    $ Aeson.object
                        [ "garment" .= Aeson.object
                            [ "circumference" .= (60 :: Int)
                            , "size" .= ("L" :: Text)
                            ]
                        ]
             in actual `shouldResolveTo` expected

        it "evaluates fragments on Query" $ do
            let sourceQuery = [r|{
              ... {
                size
              }
            }|]
             in graphql (toSchema "size" size) `shouldResolve` sourceQuery

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
            let expected = HashMap.singleton "data"
                    $ Aeson.object
                        [ "circumference" .= (60 :: Int)
                        ]
             in actual `shouldResolveTo` expected

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
            let expected = HashMap.singleton "data"
                    $ Aeson.object
                        [ "garment" .= Aeson.object
                            [ "circumference" .= (60 :: Int)
                            ]
                        ]
             in actual `shouldResolveTo` expected

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
                expected = HashMap.singleton "data"
                    $ Aeson.object
                        [ "garment" .= Aeson.object
                            [ "circumference" .= (60 :: Int)
                            ]
                        ]
            actual <- graphql (toSchema "Hat" $ garment "Hat") sourceQuery
            actual `shouldResolveTo` expected
