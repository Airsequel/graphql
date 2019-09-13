{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.FragmentSpec
    ( spec
    ) where

import Data.Aeson (object, (.=))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Language.GraphQL
import qualified Language.GraphQL.Schema as Schema
import Test.Hspec (Spec, it, shouldBe, xdescribe)
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

spec :: Spec
spec = xdescribe "Inline fragment executor" $ do
    it "chooses the first selection if the type matches" $ do
        actual <- graphql (garment "Hat" :| []) inlineQuery
        let expected = object
                [ "garment" .= object
                    [ "circumference" .= (60 :: Int)
                    ]
                ]
         in actual `shouldBe` expected

    it "chooses the last selection if the type matches" $ do
        actual <- graphql (garment "Shirt" :| []) inlineQuery
        let expected = object
                [ "garment" .= object
                    [ "size" .= ("L" :: Text)
                    ]
                ]
         in actual `shouldBe` expected
