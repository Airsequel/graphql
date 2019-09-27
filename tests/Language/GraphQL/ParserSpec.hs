{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.GraphQL.ParserSpec
    ( spec
    ) where

import Language.GraphQL.Parser (document)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldSucceedOn)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Parser" $ do
    it "accepts BOM header" $
        parse document "" `shouldSucceedOn` "\xfeff{foo}"

    it "accepts block strings as argument" $
        parse document "" `shouldSucceedOn` [r|{
              hello(text: """Argument""")
            }|]

    it "accepts strings as argument" $
        parse document "" `shouldSucceedOn` [r|{
              hello(text: "Argument")
            }|]
