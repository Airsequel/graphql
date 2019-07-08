{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.StarWars.QueryTests (test) where

import qualified Data.Aeson as Aeson
import Data.Aeson ( object
                  , (.=)
                  )
import Data.Text (Text)
import Language.GraphQL
import Language.GraphQL.Schema (Subs)
import Text.RawString.QQ (r)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( Assertion
                        , testCase
                        , (@?=)
                        )
import Test.StarWars.Schema

-- * Test
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsQueryTests.js

test :: TestTree
test = testGroup "Star Wars Query Tests"
  [ testGroup "Basic Queries"
    [ testCase "R2-D2 hero" . testQuery
        [r| query HeroNameQuery {
              hero {
                id
              }
            }
        |]
      $ object [ "data" .= object ["hero" .= object ["id" .= ("2001" :: Text)]]]
    , testCase "R2-D2 ID and friends" . testQuery
        [r| query HeroNameAndFriendsQuery {
              hero {
                id
                name
                friends {
                  name
                }
              }
            }
        |]
      $ object [ "data" .= object [
          "hero" .= object [
              "id" .= ("2001" :: Text)
            , r2d2Name
            , "friends" .= [
                  object [lukeName]
                , object [hanName]
                , object [leiaName]
                ]
            ]
        ]]
    ]
  , testGroup "Nested Queries"
    [ testCase "R2-D2 friends" . testQuery
        [r| query NestedQuery {
              hero {
                name
                friends {
                  name
                  appearsIn
                    friends {
                      name
                    }
                }
              }
            }
        |]
      $ object [ "data" .= object [
          "hero" .= object [
              "name" .= ("R2-D2" :: Text)
            , "friends" .= [
                  object [
                      "name" .= ("Luke Skywalker" :: Text)
                    , "appearsIn" .= ["NEWHOPE","EMPIRE","JEDI" :: Text]
                    , "friends" .= [
                          object [hanName]
                        , object [leiaName]
                        , object [c3poName]
                        , object [r2d2Name]
                        ]
                    ]
                , object [
                      hanName
                    , "appearsIn" .= [ "NEWHOPE","EMPIRE","JEDI" :: Text]
                    , "friends" .= [
                          object [lukeName]
                        , object [leiaName]
                        , object [r2d2Name]
                        ]
                    ]
                , object [
                      leiaName
                    , "appearsIn" .= [ "NEWHOPE","EMPIRE","JEDI" :: Text]
                    , "friends" .= [
                          object [lukeName]
                        , object [hanName]
                        , object [c3poName]
                        , object [r2d2Name]
                        ]
                    ]
                ]
            ]
        ]]
    , testCase "Luke ID" . testQuery
        [r| query FetchLukeQuery {
              human(id: "1000") {
                name
              }
            }
        |]
      $ object [ "data" .= object [
          "human" .= object [lukeName]
        ]
    ]]
    , testCase "Luke ID with variable" . testQueryParams
         (\v -> if v == "someId"
                   then Just "1000"
                   else Nothing)
         [r| query FetchSomeIDQuery($someId: String!) {
               human(id: $someId) {
                 name
               }
             }
         |]
      $ object [ "data" .= object [
          "human" .= object [lukeName]
        ]]
    , testCase "Han ID with variable" . testQueryParams
        (\v -> if v == "someId"
                  then Just "1002"
                  else Nothing)
        [r| query FetchSomeIDQuery($someId: String!) {
              human(id: $someId) {
                name
              }
            }
        |]
      $ object [ "data" .= object [
          "human" .= object [hanName]
        ]]
    , testCase "Invalid ID" . testQueryParams
        (\v -> if v == "id"
               then Just "Not a valid ID"
               else Nothing)
        [r| query humanQuery($id: String!) {
              human(id: $id) {
                name
              }
            }
        |] $ object ["data" .= object ["human" .= Aeson.Null]]
    , testCase "Luke aliased" . testQuery
        [r| query FetchLukeAliased {
              luke: human(id: "1000") {
                name
              }
            }
        |]
      $ object [ "data" .= object [
         "luke" .= object [lukeName]
        ]]
    , testCase "R2-D2 ID and friends aliased" . testQuery
        [r| query HeroNameAndFriendsQuery {
              hero {
                id
                name
                friends {
                  friendName: name
                }
              }
            }
        |]
      $ object [ "data" .= object [
          "hero" .= object [
              "id" .= ("2001" :: Text)
            , r2d2Name
            , "friends" .= [
                  object ["friendName" .= ("Luke Skywalker" :: Text)]
                , object ["friendName" .= ("Han Solo" :: Text)]
                , object ["friendName" .= ("Leia Organa" :: Text)]
                ]
            ]
        ]]
    , testCase "Luke and Leia aliased" . testQuery
        [r| query FetchLukeAndLeiaAliased {
              luke: human(id: "1000") {
                name
              }
              leia: human(id: "1003") {
                name
              }
            }
        |]
      $ object [ "data" .= object [
          "luke" .= object [lukeName]
        , "leia" .= object [leiaName]
        ]]
  , testGroup "Fragments for complex queries"
    [ testCase "Aliases to query for duplicate content" . testQuery
        [r| query DuplicateFields {
              luke: human(id: "1000") {
                name
                homePlanet
              }
              leia: human(id: "1003") {
                name
                homePlanet
              }
            }
        |]
      $ object [ "data" .= object [
          "luke" .= object [lukeName, tatooine]
        , "leia" .= object [leiaName, alderaan]
        ]]
    ,  testCase "Fragment for duplicate content" . testQuery
        [r|  query UseFragment {
              luke: human(id: "1000") {
                ...HumanFragment
              }
              leia: human(id: "1003") {
                ...HumanFragment
              }
            }
            fragment HumanFragment on Human {
              name
              homePlanet
            }
        |]
      $ object [ "data" .= object [
          "luke" .= object [lukeName, tatooine]
        , "leia" .= object [leiaName, alderaan]
        ]]
    ]
  , testGroup "__typename"
    [  testCase "R2D2 is a Droid" . testQuery
        [r| query CheckTypeOfR2 {
              hero {
                __typename
                name
              }
            }
          |]
    $ object ["data" .= object [
        "hero" .= object ["__typename" .= ("Droid" :: Text), r2d2Name]
      ]]
    , testCase "Luke is a human" . testQuery
        [r| query CheckTypeOfLuke {
              hero(episode: EMPIRE) {
                __typename
                name
              }
            }
          |]
    $ object ["data" .= object [
        "hero" .= object ["__typename" .= ("Human" :: Text), lukeName]
      ]]
    ]
    , testGroup "Errors in resolvers"
      [  testCase "error on secretBackstory" . testQuery
          [r|
            query HeroNameQuery {
              hero {
                name
                  secretBackstory
              }
            }
          |]
          $ object
              [ "data" .= object
                  [ "hero" .= object
                      [ "name" .= ("R2-D2" :: Text)
                      , "secretBackstory" .= Aeson.Null
                      ]
                  ]
              , "errors" .=
                  [ object
                      ["message" .= ("secretBackstory is secret." :: Text)]
                  ]
              ]
      , testCase "Error in a list" . testQuery
          [r| query HeroNameQuery {
                hero {
                  name
                  friends {
                    name
                    secretBackstory
                  }
                }
              }
            |]
      $ object ["data" .= object
          [ "hero" .= object
              [ "name" .= ("R2-D2" :: Text)
              , "friends" .=
                  [ object
                      [ "name" .= ("Luke Skywalker" :: Text)
                      , "secretBackstory" .= Aeson.Null
                      ]
                  , object
                      [ "name" .= ("Han Solo" :: Text)
                      , "secretBackstory" .= Aeson.Null
                      ]
                  , object
                      [ "name" .= ("Leia Organa" :: Text)
                      , "secretBackstory" .= Aeson.Null
                      ]
                  ]
              ]
          ]
        , "errors" .=
            [ object ["message" .= ("secretBackstory is secret." :: Text)]
            , object ["message" .= ("secretBackstory is secret." :: Text)]
            , object ["message" .= ("secretBackstory is secret." :: Text)]
            ]
        ]
      , testCase "error on secretBackstory with alias" . testQuery
          [r| query HeroNameQuery {
                mainHero: hero {
                  name
                  story: secretBackstory
                }
              }
            |]
          $ object
              [ "data" .= object
                  [ "mainHero" .= object
                      [ "name" .= ("R2-D2" :: Text)
                      , "story" .= Aeson.Null
                      ]
                  ]
              , "errors" .=
                  [ object ["message" .= ("secretBackstory is secret." :: Text)]
                  ]
              ]
        ]
  ]
  where
    lukeName = "name" .= ("Luke Skywalker" :: Text)
    leiaName = "name" .= ("Leia Organa" :: Text)
    hanName = "name" .= ("Han Solo" :: Text)
    r2d2Name = "name" .= ("R2-D2" :: Text)
    c3poName = "name" .= ("C-3PO" :: Text)
    tatooine = "homePlanet" .= ("Tatooine" :: Text)
    alderaan = "homePlanet" .= ("Alderaan" :: Text)

testQuery :: Text -> Aeson.Value -> Assertion
testQuery q expected = graphql schema q >>= (@?= expected)

testQueryParams :: Subs -> Text -> Aeson.Value -> Assertion
testQueryParams f q expected = graphqlSubs schema f q >>= (@?= expected)
