{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.GraphQL.LexerSpec
    ( spec
    ) where

import Data.Either (isRight)
import Data.Text (Text)
import Data.Void (Void)
import Language.GraphQL.Lexer
import Test.Hspec ( Spec
                  , context
                  , describe
                  , it
                  , shouldBe
                  , shouldSatisfy
                  )
import Text.Megaparsec ( ParseErrorBundle
                       , parse
                       )
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Lexer" $ do
    context "Reference tests" $ do
        it "accepts BOM header" $
            runParser unicodeBOM "\xfeff" `shouldSatisfy` isRight

        it "lexes strings" $ do
            runParser string [r|"simple"|] `shouldBe` Right "simple"
            runParser string [r|" white space "|] `shouldBe` Right " white space "
            runParser string [r|"quote \""|] `shouldBe` Right [r|quote "|]
            runParser string [r|"escaped \n"|] `shouldBe` Right "escaped \n"
            runParser string [r|"slashes \\ \/"|] `shouldBe` Right [r|slashes \ /|]
            runParser string [r|"unicode \u1234\u5678\u90AB\uCDEF"|]
                `shouldBe` Right "unicode ሴ噸邫췯"

        it "lexes block string" $ do
            runParser blockString [r|"""simple"""|] `shouldBe` Right "simple"
            runParser blockString [r|""" white space """|]
                `shouldBe` Right " white space "
            runParser blockString [r|"""contains " quote"""|]
                `shouldBe` Right [r|contains " quote|]
            runParser blockString [r|"""contains \""" triplequote"""|]
                `shouldBe` Right [r|contains """ triplequote|]
            runParser blockString "\"\"\"multi\nline\"\"\"" `shouldBe` Right "multi\nline"
            runParser blockString "\"\"\"multi\rline\r\nnormalized\"\"\""
                `shouldBe` Right "multi\nline\nnormalized"
            runParser blockString "\"\"\"multi\rline\r\nnormalized\"\"\""
                `shouldBe` Right "multi\nline\nnormalized"
            runParser blockString [r|"""unescaped \n\r\b\t\f\u1234"""|]
                `shouldBe` Right [r|unescaped \n\r\b\t\f\u1234|]
            runParser blockString [r|"""slashes \\ \/"""|]
                `shouldBe` Right [r|slashes \\ \/|]
            runParser blockString [r|"""

                spans
                  multiple
                    lines

                """|] `shouldBe` Right "spans\n  multiple\n    lines"

        it "lexes numbers" $ do
            runParser integer "4" `shouldBe` Right (4 :: Int)
            runParser float "4.123" `shouldBe` Right 4.123
            runParser integer "-4" `shouldBe` Right (-4 :: Int)
            runParser integer "9" `shouldBe` Right (9 :: Int)
            runParser integer "0" `shouldBe` Right (0 :: Int)
            runParser float "-4.123" `shouldBe` Right (-4.123)
            runParser float "0.123" `shouldBe` Right 0.123
            runParser float "123e4" `shouldBe` Right 123e4
            runParser float "123E4" `shouldBe` Right 123E4
            runParser float "123e-4" `shouldBe` Right 123e-4
            runParser float "123e+4" `shouldBe` Right 123e+4
            runParser float "-1.123e4" `shouldBe` Right (-1.123e4)
            runParser float "-1.123E4" `shouldBe` Right (-1.123E4)
            runParser float "-1.123e-4" `shouldBe` Right (-1.123e-4)
            runParser float "-1.123e+4" `shouldBe` Right (-1.123e+4)
            runParser float "-1.123e4567" `shouldBe` Right (-1.123e4567)

        it "lexes punctuation" $ do
            runParser bang "!" `shouldBe` Right '!'
            runParser dollar "$" `shouldBe` Right '$'
            runBetween parens "()" `shouldSatisfy` isRight
            runParser spread "..." `shouldBe` Right "..."
            runParser colon ":" `shouldBe` Right ":"
            runParser equals "=" `shouldBe` Right "="
            runParser at "@" `shouldBe` Right '@'
            runBetween brackets "[]" `shouldSatisfy` isRight
            runBetween braces "{}" `shouldSatisfy` isRight
            runParser pipe "|" `shouldBe` Right "|"

    context "Implementation tests" $ do
        it "lexes empty block strings" $
            runParser blockString [r|""""""|] `shouldBe` Right ""
        it "lexes ampersand" $
            runParser amp "&" `shouldBe` Right "&"

runParser :: forall a. Parser a -> Text -> Either (ParseErrorBundle Text Void) a
runParser = flip parse ""

runBetween :: (Parser () -> Parser ()) -> Text -> Either (ParseErrorBundle Text Void) ()
runBetween parser = parse (parser $ pure ()) ""
