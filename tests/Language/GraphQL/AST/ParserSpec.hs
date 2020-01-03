{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.GraphQL.AST.ParserSpec
    ( spec
    ) where

import Language.GraphQL.AST.Parser
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

    it "accepts two required arguments" $
        parse document "" `shouldSucceedOn` [r|
            mutation auth($username: String!, $password: String!){
                test
            }|]

    it "accepts two string arguments" $
        parse document "" `shouldSucceedOn` [r|
            mutation auth{
                test(username: "username", password: "password")
            }|]

    it "accepts two block string arguments" $
        parse document "" `shouldSucceedOn` [r|
            mutation auth{
                test(username: """username""", password: """password""")
            }|]

    it "parses minimal schema definition" $
        parse document "" `shouldSucceedOn` [r|schema { query: Query }|]
