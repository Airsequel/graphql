{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.GraphQL.AST.EncoderSpec
    ( spec
    ) where

import Language.GraphQL.AST
import Language.GraphQL.AST.Encoder
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldStartWith, shouldEndWith, shouldNotContain)
import Test.QuickCheck (choose, oneof, forAll)
import Text.RawString.QQ (r)
import Data.Text.Lazy (cons, toStrict, unpack)

spec :: Spec
spec = do
    describe "value" $ do
        context "null value" $ do
            let testNull formatter = value formatter Null `shouldBe` "null"
            it "minified" $ testNull minified
            it "pretty" $ testNull pretty

        context "minified" $ do
            it "escapes \\" $
                value minified (String "\\") `shouldBe` "\"\\\\\""
            it "escapes double quotes" $
                value minified (String "\"") `shouldBe` "\"\\\"\""
            it "escapes \\f" $
                value minified (String "\f") `shouldBe` "\"\\f\""
            it "escapes \\n" $
                value minified (String "\n") `shouldBe` "\"\\n\""
            it "escapes \\r" $
                value minified (String "\r") `shouldBe` "\"\\r\""
            it "escapes \\t" $
                value minified (String "\t") `shouldBe` "\"\\t\""
            it "escapes backspace" $
                value minified (String "a\bc") `shouldBe` "\"a\\bc\""
            context "escapes Unicode for chars less than 0010" $ do
                it "Null" $ value minified (String "\x0000") `shouldBe` "\"\\u0000\""
                it "bell" $ value minified (String "\x0007") `shouldBe` "\"\\u0007\""
            context "escapes Unicode for char less than 0020" $ do
                it "DLE" $ value minified (String "\x0010") `shouldBe` "\"\\u0010\""
                it "EM" $ value minified (String "\x0019") `shouldBe` "\"\\u0019\""
            context "encodes without escape" $ do
                it "space" $ value minified (String "\x0020") `shouldBe` "\" \""
                it "~" $ value minified (String "\x007E") `shouldBe` "\"~\""

        context "pretty" $ do
            it "uses strings for short string values" $
                value pretty (String "Short text") `shouldBe` "\"Short text\""
            it "uses block strings for text with new lines, with newline symbol" $
                value pretty (String "Line 1\nLine 2")
                    `shouldBe` [r|"""
  Line 1
  Line 2
"""|]
            it "uses block strings for text with new lines, with CR symbol" $
                value pretty (String "Line 1\rLine 2")
                    `shouldBe` [r|"""
  Line 1
  Line 2
"""|]
            it "uses block strings for text with new lines, with CR symbol followed by newline" $
                value pretty (String "Line 1\r\nLine 2")
                    `shouldBe` [r|"""
  Line 1
  Line 2
"""|]
            it "encodes as one line string if has escaped symbols" $ do
                let
                  genNotAllowedSymbol = oneof
                    [ choose ('\x0000', '\x0008')
                    , choose ('\x000B', '\x000C')
                    , choose ('\x000E', '\x001F')
                    , pure '\x007F'
                    ]

                forAll genNotAllowedSymbol $ \x -> do
                    let
                      rawValue = "Short \n" <> cons x "text"
                      encoded = value pretty (String $ toStrict rawValue)
                    shouldStartWith (unpack encoded) "\""
                    shouldEndWith (unpack encoded) "\""
                    shouldNotContain (unpack encoded) "\"\"\""

            it "Hello world" $ value pretty (String "Hello,\n  World!\n\nYours,\n  GraphQL.")
              `shouldBe` [r|"""
  Hello,
    World!

  Yours,
    GraphQL.
"""|]

            it "has only newlines" $ value pretty (String "\n") `shouldBe` [r|"""


"""|]
            it "has newlines and one symbol at the begining" $
              value pretty (String "a\n\n") `shouldBe` [r|"""
  a


"""|]
            it "has newlines and one symbol at the end" $
              value pretty (String "\n\na") `shouldBe` [r|"""


  a
"""|]
            it "has newlines and one symbol in the middle" $
              value pretty (String "\na\n") `shouldBe` [r|"""

  a

"""|]
            it "skip trailing whitespaces" $ value pretty (String "  Short\ntext    ")
              `shouldBe` [r|"""
  Short
  text
"""|]

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
