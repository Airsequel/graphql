{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.GraphQL.LexerTest
    ( implementation
    , reference
    ) where

import Control.Applicative (Alternative(..))
import Language.GraphQL.Lexer
import qualified Data.Text as T
import Data.Void (Void)
import Test.Tasty ( TestTree
                  , testGroup
                  )
import Test.Tasty.HUnit ( testCase
                        , (@?=)
                        )
import Text.Megaparsec ( ParseErrorBundle
                       , parse
                       )
import Text.RawString.QQ (r)

reference :: TestTree
reference = testGroup "Lexer"
    [ testCase "lexes strings" $ do
        runParser string [r|"simple"|] @?= Right "simple"
        runParser string [r|" white space "|] @?= Right " white space "
        runParser string [r|"quote \""|] @?= Right [r|quote "|]
        runParser string [r|"escaped \n"|] @?= Right "escaped \n"
        runParser string [r|"slashes \\ \/"|] @?= Right [r|slashes \ /|]
        runParser string [r|"unicode \u1234\u5678\u90AB\uCDEF"|]
            @?= Right "unicode ሴ噸邫췯"

    , testCase "lexes block string" $ do
        runParser blockString [r|"""simple"""|] @?= Right "simple"
        runParser blockString [r|""" white space """|]
            @?= Right " white space "
        runParser blockString [r|"""contains " quote"""|]
            @?= Right [r|contains " quote|]
        runParser blockString [r|"""contains \""" triplequote"""|]
            @?= Right [r|contains """ triplequote|]
        runParser blockString "\"\"\"multi\nline\"\"\"" @?= Right "multi\nline"
        runParser blockString "\"\"\"multi\rline\r\nnormalized\"\"\""
            @?= Right "multi\nline\nnormalized"
        runParser blockString "\"\"\"multi\rline\r\nnormalized\"\"\""
            @?= Right "multi\nline\nnormalized"
        runParser blockString [r|"""unescaped \n\r\b\t\f\u1234"""|]
            @?= Right [r|unescaped \n\r\b\t\f\u1234|]
        runParser blockString [r|"""slashes \\ \/"""|]
            @?= Right [r|slashes \\ \/|]
        runParser blockString [r|"""

            spans
              multiple
                lines

            """|] @?= Right "spans\n  multiple\n    lines"

    , testCase "lexes numbers" $ do
        runParser integer "4" @?= Right 4
        runParser float "4.123" @?= Right 4.123
        runParser integer "-4" @?= Right (-4)
        runParser integer "9" @?= Right 9
        runParser integer "0" @?= Right 0
        runParser float "-4.123" @?= Right (-4.123)
        runParser float "0.123" @?= Right 0.123
        runParser float "123e4" @?= Right 123e4
        runParser float "123E4" @?= Right 123E4
        runParser float "123e-4" @?= Right 123e-4
        runParser float "123e+4" @?= Right 123e+4
        runParser float "-1.123e4" @?= Right (-1.123e4)
        runParser float "-1.123E4" @?= Right (-1.123E4)
        runParser float "-1.123e-4" @?= Right (-1.123e-4)
        runParser float "-1.123e+4" @?= Right (-1.123e+4)
        runParser float "-1.123e4567" @?= Right (-1.123e4567)

    , testCase "lexes punctuation" $ do
        runParser bang "!" @?= Right '!'
        runParser dollar "$" @?= Right '$'
        runBetween parens "()" @?= Right ()
        runParser spread "..." @?= Right "..."
        runParser colon ":" @?= Right ":"
        runParser equals "=" @?= Right "="
        runParser at "@" @?= Right '@'
        runBetween brackets "[]" @?= Right ()
        runBetween braces "{}" @?= Right ()
        runParser pipe "|" @?= Right "|"
    ]

implementation :: TestTree
implementation = testGroup "Lexer"
    [ testCase "lexes empty block strings" $
        runParser blockString [r|""""""|] @?= Right ""
    , testCase "lexes ampersand" $
        runParser amp "&" @?= Right "&"
    ]

runParser :: forall a. Parser a -> T.Text -> Either (ParseErrorBundle T.Text Void) a
runParser = flip parse ""

runBetween :: (Parser () -> Parser ()) -> T.Text -> Either (ParseErrorBundle T.Text Void) ()
runBetween parser = parse (parser $ pure ()) ""
