{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.GraphQL.AST.EncoderSpec
    ( spec
    ) where

import Language.GraphQL.AST
import Language.GraphQL.AST.Encoder
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = do
    describe "value" $ do
        context "minified" $ do
            it "escapes \\" $
                value minified (String "\\") `shouldBe` "\"\\\\\""
            it "escapes quotes" $
                value minified (String "\"") `shouldBe` "\"\\\"\""
            it "escapes backspace" $
                value minified (String "a\bc") `shouldBe` "\"a\\bc\""
            it "escapes Unicode" $
                value minified (String "\0") `shouldBe` "\"\\u0000\""

        context "pretty" $ do
            it "uses strings for short string values" $
                value pretty (String "Short text") `shouldBe` "\"Short text\""
            it "uses block strings for text with new lines" $
                value pretty (String "Line 1\nLine 2")
                    `shouldBe` "\"\"\"\n  Line 1\n  Line 2\n\"\"\""
            it "escapes \\ in short strings" $
                value pretty (String "\\") `shouldBe` "\"\\\\\""

    describe "definition" $
        it "indents block strings in arguments" $
            let arguments = [Argument "message" (String "line1\nline2")]
                field = Field Nothing "field" arguments [] []
                operation = DefinitionOperation $ SelectionSet $ pure field
             in definition pretty operation `shouldBe` [r|{
  field(message: """
    line1
    line2
  """)
}
|]
