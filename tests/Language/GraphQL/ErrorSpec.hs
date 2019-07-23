{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.ErrorSpec
    ( spec
    ) where

import qualified Data.Aeson as Aeson
import Language.GraphQL.Error
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  )

spec :: Spec
spec = describe "singleError" $
    it "constructs an error with the given message" $
        let expected = Aeson.object
                [
                    ("errors", Aeson.toJSON
                        [ Aeson.object [("message", "Message.")]
                        ]
                    )
                ]
         in singleError "Message." `shouldBe` expected
