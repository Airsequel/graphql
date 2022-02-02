{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.GraphQL.AST.ParserSpec
    ( spec
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Language.GraphQL.AST.Document
import qualified Language.GraphQL.AST.DirectiveLocation as DirLoc
import Language.GraphQL.AST.Parser
import Language.GraphQL.TH
import Test.Hspec (Spec, describe, it, context)
import Test.Hspec.Megaparsec (shouldParse, shouldFailOn, shouldSucceedOn)
import Text.Megaparsec (parse)
import Test.QuickCheck (property, NonEmptyList (..), mapSize)
import Language.GraphQL.AST.Arbitrary

spec :: Spec
spec = describe "Parser" $ do
    it "accepts BOM header" $
        parse document "" `shouldSucceedOn` "\xfeff{foo}"

    context "Arguments" $ do
       it "accepts block strings as argument" $
           parse document "" `shouldSucceedOn` [gql|{
                 hello(text: """Argument""")
               }|]

       it "accepts strings as argument" $
           parse document "" `shouldSucceedOn` [gql|{
                 hello(text: "Argument")
               }|]

       it "accepts int as argument1" $
           parse document "" `shouldSucceedOn` [gql|{
                 user(id: 4)
               }|]

       it "accepts boolean as argument" $
           parse document "" `shouldSucceedOn` [gql|{
                 hello(flag: true) { field1 }
               }|]

       it "accepts float as argument" $
           parse document "" `shouldSucceedOn` [gql|{
                 body(height: 172.5) { height }
               }|]

       it "accepts empty list as argument" $
           parse document "" `shouldSucceedOn` [gql|{
                 query(list: []) { field1 }
               }|]

       it "accepts two required arguments" $
           parse document "" `shouldSucceedOn` [gql|
               mutation auth($username: String!, $password: String!){
                 test
               }|]

       it "accepts two string arguments" $
           parse document "" `shouldSucceedOn` [gql|
               mutation auth{
                 test(username: "username", password: "password")
               }|]

       it "accepts two block string arguments" $
           parse document "" `shouldSucceedOn` [gql|
               mutation auth{
                 test(username: """username""", password: """password""")
               }|]
 
       it "accepts any arguments" $ mapSize (const 10) $ property $ \xs ->
            let
                query' :: Text
                arguments = map printArgument $ getNonEmpty (xs :: NonEmptyList (AnyArgument AnyValue))
                query' = "query(" <> Text.intercalate ", " arguments <> ")" in
            parse document "" `shouldSucceedOn` ("{ " <> query' <> " }")

    it "parses minimal schema definition" $
        parse document "" `shouldSucceedOn` [gql|schema { query: Query }|]

    it "parses minimal scalar definition" $
        parse document "" `shouldSucceedOn` [gql|scalar Time|]

    it "parses ImplementsInterfaces" $
        parse document "" `shouldSucceedOn` [gql|
            type Person implements NamedEntity & ValuedEntity {
              name: String
            }
        |]

    it "parses a  type without ImplementsInterfaces" $
        parse document "" `shouldSucceedOn` [gql|
            type Person {
              name: String
            }
        |]

    it "parses ArgumentsDefinition in an ObjectDefinition" $
        parse document "" `shouldSucceedOn` [gql|
            type Person {
              name(first: String, last: String): String
            }
        |]

    it "parses minimal union type definition" $
        parse document "" `shouldSucceedOn` [gql|
            union SearchResult = Photo | Person
        |]

    it "parses minimal interface type definition" $
        parse document "" `shouldSucceedOn` [gql|
            interface NamedEntity {
              name: String
            }
        |]

    it "parses minimal enum type definition" $
        parse document "" `shouldSucceedOn` [gql|
            enum Direction {
              NORTH
              EAST
              SOUTH
              WEST
            }
        |]

    it "parses minimal input object type definition" $
        parse document "" `shouldSucceedOn` [gql|
            input Point2D {
              x: Float
              y: Float
            }
        |]

    it "parses minimal input enum definition with an optional pipe" $
        parse document "" `shouldSucceedOn` [gql|
            directive @example on
              | FIELD
              | FRAGMENT_SPREAD
        |]

    it "parses two minimal directive definitions" $
        let directive nm loc =
                TypeSystemDefinition
                    (DirectiveDefinition
                         (Description Nothing)
                         nm
                         (ArgumentsDefinition [])
                         (loc :| []))
            example1 =
                directive "example1"
                    (DirLoc.TypeSystemDirectiveLocation DirLoc.FieldDefinition)
                    (Location {line = 1, column = 1})
            example2 =
                directive "example2"
                    (DirLoc.ExecutableDirectiveLocation DirLoc.Field)
                    (Location {line = 2, column = 1})
            testSchemaExtension = example1 :| [ example2 ]
            query = [gql|
              directive @example1 on FIELD_DEFINITION
              directive @example2 on FIELD
            |]
         in parse document "" query `shouldParse` testSchemaExtension

    it "parses a directive definition with a default empty list argument" $
        let directive nm loc args =
                TypeSystemDefinition
                    (DirectiveDefinition
                         (Description Nothing)
                         nm
                         (ArgumentsDefinition
                              [ InputValueDefinition
                                    (Description Nothing)
                                    argName
                                    argType
                                    argValue
                                    []
                              | (argName, argType, argValue) <- args])
                         (loc :| []))
            defn =
                directive "test"
                    (DirLoc.TypeSystemDirectiveLocation DirLoc.FieldDefinition)
                    [("foo",
                      TypeList (TypeNamed "String"),
                      Just
                          $ Node (ConstList [])
                          $ Location {line = 1, column = 33})]
                    (Location {line = 1, column = 1})
            query = [gql|directive @test(foo: [String] = []) on FIELD_DEFINITION|]
         in parse document "" query `shouldParse` (defn :| [ ])

    it "parses schema extension with a new directive" $
        parse document "" `shouldSucceedOn`[gql|
            extend schema @newDirective
        |]

    it "parses schema extension with an operation type definition" $
        parse document "" `shouldSucceedOn` [gql|extend schema { query: Query }|]

    it "parses schema extension with an operation type and directive" $
        let newDirective = Directive "newDirective" [] $ Location 1 15
            schemaExtension = SchemaExtension
                $ SchemaOperationExtension [newDirective]
                $ OperationTypeDefinition Query "Query" :| []
            testSchemaExtension = TypeSystemExtension schemaExtension
                $ Location 1 1
            query = [gql|extend schema @newDirective { query: Query }|]
         in parse document "" query `shouldParse` (testSchemaExtension :| [])

    it "parses an object extension" $
        parse document "" `shouldSucceedOn` [gql|
            extend type Story {
              isHiddenLocally: Boolean
            }
        |]

    it "rejects variables in DefaultValue" $
        parse document "" `shouldFailOn` [gql|
            query ($book: String = "Zarathustra", $author: String = $book) {
              title
            }
        |]

    it "rejects empty selection set" $
       parse document "" `shouldFailOn` [gql|
            query {
              innerField {}
            }
        |]

    it "parses documents beginning with a comment" $
        parse document "" `shouldSucceedOn` [gql|
            """
            Query
            """
            type Query {
                queryField: String
            }
        |]

    it "parses subscriptions" $
        parse document "" `shouldSucceedOn` [gql|
            subscription NewMessages {
              newMessage(roomId: 123) {
                sender
              }
            }
        |]
