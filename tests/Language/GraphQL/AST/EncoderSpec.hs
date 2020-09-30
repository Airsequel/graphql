{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.GraphQL.AST.EncoderSpec
    ( spec
    ) where

import qualified Language.GraphQL.AST.Document as Full
import Language.GraphQL.AST.Encoder
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldStartWith, shouldEndWith, shouldNotContain)
import Test.QuickCheck (choose, oneof, forAll)
import Text.RawString.QQ (r)
import Data.Text.Lazy (cons, toStrict, unpack)

spec :: Spec
spec = do
    describe "value" $ do
        context "null value" $ do
            let testNull formatter = value formatter Full.Null `shouldBe` "null"
            it "minified" $ testNull minified
            it "pretty" $ testNull pretty

        context "minified" $ do
            it "escapes \\" $
                value minified (Full.String "\\") `shouldBe` "\"\\\\\""
            it "escapes double quotes" $
                value minified (Full.String "\"") `shouldBe` "\"\\\"\""
            it "escapes \\f" $
                value minified (Full.String "\f") `shouldBe` "\"\\f\""
            it "escapes \\n" $
                value minified (Full.String "\n") `shouldBe` "\"\\n\""
            it "escapes \\r" $
                value minified (Full.String "\r") `shouldBe` "\"\\r\""
            it "escapes \\t" $
                value minified (Full.String "\t") `shouldBe` "\"\\t\""
            it "escapes backspace" $
                value minified (Full.String "a\bc") `shouldBe` "\"a\\bc\""
            context "escapes Unicode for chars less than 0010" $ do
                it "Null" $ value minified (Full.String "\x0000") `shouldBe` "\"\\u0000\""
                it "bell" $ value minified (Full.String "\x0007") `shouldBe` "\"\\u0007\""
            context "escapes Unicode for char less than 0020" $ do
                it "DLE" $ value minified (Full.String "\x0010") `shouldBe` "\"\\u0010\""
                it "EM" $ value minified (Full.String "\x0019") `shouldBe` "\"\\u0019\""
            context "encodes without escape" $ do
                it "space" $ value minified (Full.String "\x0020") `shouldBe` "\" \""
                it "~" $ value minified (Full.String "\x007E") `shouldBe` "\"~\""

        context "pretty" $ do
            it "uses strings for short string values" $
                value pretty (Full.String "Short text") `shouldBe` "\"Short text\""
            it "uses block strings for text with new lines, with newline symbol" $
                value pretty (Full.String "Line 1\nLine 2")
                    `shouldBe` [r|"""
  Line 1
  Line 2
"""|]
            it "uses block strings for text with new lines, with CR symbol" $
                value pretty (Full.String "Line 1\rLine 2")
                    `shouldBe` [r|"""
  Line 1
  Line 2
"""|]
            it "uses block strings for text with new lines, with CR symbol followed by newline" $
                value pretty (Full.String "Line 1\r\nLine 2")
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
                      encoded = value pretty (Full.String $ toStrict rawValue)
                    shouldStartWith (unpack encoded) "\""
                    shouldEndWith (unpack encoded) "\""
                    shouldNotContain (unpack encoded) "\"\"\""

            it "Hello world" $ value pretty (Full.String "Hello,\n  World!\n\nYours,\n  GraphQL.")
              `shouldBe` [r|"""
  Hello,
    World!

  Yours,
    GraphQL.
"""|]

            it "has only newlines" $ value pretty (Full.String "\n") `shouldBe` [r|"""


"""|]
            it "has newlines and one symbol at the begining" $
              value pretty (Full.String "a\n\n") `shouldBe` [r|"""
  a


"""|]
            it "has newlines and one symbol at the end" $
              value pretty (Full.String "\n\na") `shouldBe` [r|"""


  a
"""|]
            it "has newlines and one symbol in the middle" $
              value pretty (Full.String "\na\n") `shouldBe` [r|"""

  a

"""|]
            it "skip trailing whitespaces" $ value pretty (Full.String "  Short\ntext    ")
              `shouldBe` [r|"""
  Short
  text
"""|]

    describe "definition" $
        it "indents block strings in arguments" $
            let location = Full.Location 0 0
                argumentValue = Full.Node (Full.String "line1\nline2") location
                arguments = [Full.Argument "message" argumentValue location]
                field = Full.Field Nothing "field" arguments [] [] location
                fieldSelection = pure $ Full.FieldSelection field
                operation = Full.DefinitionOperation
                    $ Full.SelectionSet fieldSelection location
             in definition pretty operation `shouldBe` [r|{
  field(message: """
    line1
    line2
  """)
}
|]
