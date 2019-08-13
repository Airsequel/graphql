{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.EncoderSpec
    ( spec
    ) where

import Language.GraphQL.AST ( Value(..))
import Language.GraphQL.Encoder ( value
                                , minified
                                )
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  )

spec :: Spec
spec = describe "value" $ do
    it "escapes \\" $
        value minified (ValueString "\\") `shouldBe` "\"\\\\\""
    it "escapes quotes" $
        value minified (ValueString "\"") `shouldBe` "\"\\\"\""
