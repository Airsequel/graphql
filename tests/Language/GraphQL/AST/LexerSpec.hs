{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.GraphQL.AST.LexerSpec
    ( spec
    ) where

import Data.Text (Text)
import Data.Void (Void)
import Language.GraphQL.AST.Lexer
import Test.Hspec (Spec, context, describe, it)
import Test.Hspec.Megaparsec (shouldParse, shouldSucceedOn)
import Text.Megaparsec (ParseErrorBundle, parse)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Lexer" $ do
    context "Reference tests" $ do
        it "accepts BOM header" $
            parse unicodeBOM "" `shouldSucceedOn` "\xfeff"

        it "lexes strings" $ do
            parse string "" [r|"simple"|] `shouldParse` "simple"
            parse string "" [r|" white space "|] `shouldParse` " white space "
            parse string "" [r|"quote \""|] `shouldParse` [r|quote "|]
            parse string "" [r|"escaped \n"|] `shouldParse` "escaped \n"
            parse string "" [r|"slashes \\ \/"|] `shouldParse` [r|slashes \ /|]
            parse string "" [r|"unicode \u1234\u5678\u90AB\uCDEF"|]
                `shouldParse` "unicode ሴ噸邫췯"

        it "lexes block string" $ do
            parse blockString "" [r|"""simple"""|] `shouldParse` "simple"
            parse blockString "" [r|""" white space """|]
                `shouldParse` " white space "
            parse blockString "" [r|"""contains " quote"""|]
                `shouldParse` [r|contains " quote|]
            parse blockString "" [r|"""contains \""" triplequote"""|]
                `shouldParse` [r|contains """ triplequote|]
            parse blockString "" "\"\"\"multi\nline\"\"\"" `shouldParse` "multi\nline"
            parse blockString "" "\"\"\"multi\rline\r\nnormalized\"\"\""
                `shouldParse` "multi\nline\nnormalized"
            parse blockString "" "\"\"\"multi\rline\r\nnormalized\"\"\""
                `shouldParse` "multi\nline\nnormalized"
            parse blockString "" [r|"""unescaped \n\r\b\t\f\u1234"""|]
                `shouldParse` [r|unescaped \n\r\b\t\f\u1234|]
            parse blockString "" [r|"""slashes \\ \/"""|]
                `shouldParse` [r|slashes \\ \/|]
            parse blockString "" [r|"""

                spans
                  multiple
                    lines

                """|] `shouldParse` "spans\n  multiple\n    lines"

        it "lexes numbers" $ do
            parse integer "" "4" `shouldParse` (4 :: Int)
            parse float "" "4.123" `shouldParse` 4.123
            parse integer "" "-4" `shouldParse` (-4 :: Int)
            parse integer "" "9" `shouldParse` (9 :: Int)
            parse integer "" "0" `shouldParse` (0 :: Int)
            parse float "" "-4.123" `shouldParse` (-4.123)
            parse float "" "0.123" `shouldParse` 0.123
            parse float "" "123e4" `shouldParse` 123e4
            parse float "" "123E4" `shouldParse` 123E4
            parse float "" "123e-4" `shouldParse` 123e-4
            parse float "" "123e+4" `shouldParse` 123e+4
            parse float "" "-1.123e4" `shouldParse` (-1.123e4)
            parse float "" "-1.123E4" `shouldParse` (-1.123E4)
            parse float "" "-1.123e-4" `shouldParse` (-1.123e-4)
            parse float "" "-1.123e+4" `shouldParse` (-1.123e+4)
            parse float "" "-1.123e4567" `shouldParse` (-1.123e4567)

        it "lexes punctuation" $ do
            parse bang "" "!" `shouldParse` "!"
            parse dollar "" "$" `shouldParse` "$"
            runBetween parens `shouldSucceedOn` "()"
            parse spread "" "..." `shouldParse` "..."
            parse colon "" ":" `shouldParse` ":"
            parse equals "" "=" `shouldParse` "="
            parse at "" "@" `shouldParse` '@'
            runBetween brackets `shouldSucceedOn` "[]"
            runBetween braces `shouldSucceedOn` "{}"
            parse pipe "" "|" `shouldParse` "|"

    context "Implementation tests" $ do
        it "lexes empty block strings" $
            parse blockString "" [r|""""""|] `shouldParse` ""
        it "lexes ampersand" $
            parse amp "" "&" `shouldParse` "&"

runBetween :: (Parser () -> Parser ()) -> Text -> Either (ParseErrorBundle Text Void) ()
runBetween parser = parse (parser $ pure ()) ""
