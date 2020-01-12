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

    it "parses minimal scalar definition" $
        parse document "" `shouldSucceedOn` [r|scalar Time|]

    it "parses ImplementsInterfaces" $
        parse document "" `shouldSucceedOn` [r|
            type Person implements NamedEntity & ValuedEntity {
              name: String
            }
        |]

    it "parses a  type without ImplementsInterfaces" $
        parse document "" `shouldSucceedOn` [r|
            type Person {
              name: String
            }
        |]

    it "parses ArgumentsDefinition in an ObjectDefinition" $
        parse document "" `shouldSucceedOn` [r|
            type Person {
              name(first: String, last: String): String
            }
        |]

    it "parses minimal union type definition" $
        parse document "" `shouldSucceedOn` [r|
            union SearchResult = Photo | Person
        |]

    it "parses minimal interface type definition" $
        parse document "" `shouldSucceedOn` [r|
            interface NamedEntity {
              name: String
            }
        |]

    it "parses minimal enum type definition" $
        parse document "" `shouldSucceedOn` [r|
            enum Direction {
              NORTH
              EAST
              SOUTH
              WEST
            }
        |]

    it "parses minimal enum type definition" $
        parse document "" `shouldSucceedOn` [r|
            enum Direction {
              NORTH
              EAST
              SOUTH
              WEST
            }
        |]

    it "parses minimal input object type definition" $
        parse document "" `shouldSucceedOn` [r|
            input Point2D {
              x: Float
              y: Float
            }
        |]
