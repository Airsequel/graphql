{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.FragmentSpec
    ( spec
    ) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Language.GraphQL
import qualified Language.GraphQL.Schema as Schema
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  , shouldSatisfy
                  , shouldNotSatisfy
                  )
import Text.RawString.QQ (r)

size :: Schema.Resolver IO
size = Schema.scalar "size" $ return ("L" :: Text)

circumference :: Schema.Resolver IO
circumference = Schema.scalar "circumference" $ return (60 :: Int)

garment :: Text -> Schema.Resolver IO
garment typeName = Schema.object "garment" $ return
    [ if typeName == "Hat" then circumference else size
    , Schema.scalar "__typename" $ return typeName
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

spec :: Spec
spec = do
    describe "Inline fragment executor" $ do
        it "chooses the first selection if the type matches" $ do
            actual <- graphql (garment "Hat" :| []) inlineQuery
            let expected = object
                    [ "data" .= object
                        [ "garment" .= object
                            [ "circumference" .= (60 :: Int)
                            ]
                        ]
                    ]
             in actual `shouldBe` expected

        it "chooses the last selection if the type matches" $ do
            actual <- graphql (garment "Shirt" :| []) inlineQuery
            let expected = object
                    [ "data" .= object
                        [ "garment" .= object
                            [ "size" .= ("L" :: Text)
                            ]
                        ]
                    ]
             in actual `shouldBe` expected

        it "embeds inline fragments without type" $ do
            let query = [r|{
              garment {
                circumference
                ... {
                  size
                }
              }
            }|]
                resolvers = Schema.object "garment" $ return [circumference,  size]

            actual <- graphql (resolvers :| []) query
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
            let query = [r|{
              ... {
                size
              }
            }|]

            actual <- graphql (size :| []) query
            actual `shouldNotSatisfy` hasErrors

    describe "Fragment spread executor" $ do
        it "evaluates fragment spreads" $ do
            let query = [r|
              {
                ...circumferenceFragment
              }

              fragment circumferenceFragment on Hat {
                circumference
              }
            |]

            actual <- graphql (circumference :| []) query
            let expected = object
                    [ "data" .= object
                        [ "circumference" .= (60 :: Int)
                        ]
                    ]
             in actual `shouldBe` expected

        it "evaluates nested fragments" $ do
            let query = [r|
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

            actual <- graphql (garment "Hat" :| []) query
            let expected = object
                    [ "data" .= object
                        [ "garment" .= object
                            [ "circumference" .= (60 :: Int)
                            ]
                        ]
                    ]
             in actual `shouldBe` expected

        it "rejects recursive fragments" $ do
            let query = [r|
              {
                ...circumferenceFragment
              }

              fragment circumferenceFragment on Hat {
                ...circumferenceFragment
              }
            |]

            actual <- graphql (circumference :| []) query
            actual `shouldSatisfy` hasErrors

        it "considers type condition" $ do
            let query = [r|
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
            actual <- graphql (garment "Hat" :| []) query
            actual `shouldBe` expected
