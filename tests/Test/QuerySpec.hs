{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.QuerySpec
    ( spec
    ) where

import Data.Aeson ((.=), object)
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty(..))
import Language.GraphQL
import qualified Language.GraphQL.Schema as Schema
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec =
    describe "Query executor" $
        it "returns objects from the root resolvers" $ do
            let query = [r|
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
            actual <- graphql schema query
            actual `shouldBe` expected
          where
            schema = HashMap.singleton "Query" $ garment' :| []
            garment' = Schema.object "garment" $ return
                [ circumference'
                ]
            circumference' = Schema.scalar "circumference" $ pure (60 :: Int)
