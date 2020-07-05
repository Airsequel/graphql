{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.StarWars.QuerySpec
    ( spec
    ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Functor.Identity (Identity(..))
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Language.GraphQL
import Text.RawString.QQ (r)
import Test.Hspec.Expectations (Expectation, shouldBe)
import Test.Hspec (Spec, describe, it)
import Test.StarWars.Schema

-- * Test
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsQueryTests.js

spec :: Spec
spec = describe "Star Wars Query Tests" $ do
    describe "Basic Queries" $ do
      it "R2-D2 hero" $ testQuery
        [r| query HeroNameQuery {
            hero {
                id
            }
            }
        |]
        $ Aeson.object
            [ "data" .= Aeson.object
                [ "hero" .= Aeson.object ["id" .= ("2001" :: Text)]
                ]
            ]
      it "R2-D2 ID and friends" $ testQuery
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
        $ Aeson.object [ "data" .= Aeson.object [
            "hero" .= Aeson.object
                [ "id" .= ("2001" :: Text)
                , r2d2Name
                , "friends" .=
                    [ Aeson.object [lukeName]
                    , Aeson.object [hanName]
                    , Aeson.object [leiaName]
                    ]
                ]
        ]]

    describe "Nested Queries" $ do
      it "R2-D2 friends" $ testQuery
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
        $ Aeson.object [ "data" .= Aeson.object [
          "hero" .= Aeson.object [
              "name" .= ("R2-D2" :: Text)
            , "friends" .= [
                  Aeson.object [
                      "name" .= ("Luke Skywalker" :: Text)
                    , "appearsIn" .= ["NEW_HOPE", "EMPIRE", "JEDI" :: Text]
                    , "friends" .= [
                          Aeson.object [hanName]
                        , Aeson.object [leiaName]
                        , Aeson.object [c3poName]
                        , Aeson.object [r2d2Name]
                        ]
                    ]
                , Aeson.object [
                      hanName
                    , "appearsIn" .= ["NEW_HOPE", "EMPIRE", "JEDI" :: Text]
                    , "friends" .=
                        [ Aeson.object [lukeName]
                        , Aeson.object [leiaName]
                        , Aeson.object [r2d2Name]
                        ]
                    ]
                , Aeson.object [
                      leiaName
                    , "appearsIn" .= ["NEW_HOPE", "EMPIRE", "JEDI" :: Text]
                    , "friends" .=
                        [ Aeson.object [lukeName]
                        , Aeson.object [hanName]
                        , Aeson.object [c3poName]
                        , Aeson.object [r2d2Name]
                        ]
                    ]
                ]
            ]
        ]]
      it "Luke ID" $ testQuery
        [r| query FetchLukeQuery {
              human(id: "1000") {
                name
              }
            }
        |]
        $ Aeson.object [ "data" .= Aeson.object
            [ "human" .= Aeson.object [lukeName]
            ]]

    it "Luke ID with variable" $ testQueryParams
      (HashMap.singleton "someId" "1000")
      [r| query FetchSomeIDQuery($someId: String!) {
            human(id: $someId) {
              name
            }
          }
      |]
      $ Aeson.object [ "data" .= Aeson.object [
        "human" .= Aeson.object [lukeName]
      ]]
    it "Han ID with variable" $ testQueryParams
      (HashMap.singleton "someId" "1002")
      [r| query FetchSomeIDQuery($someId: String!) {
            human(id: $someId) {
              name
            }
          }
      |]
      $ Aeson.object [ "data" .= Aeson.object [
        "human" .= Aeson.object [hanName]
      ]]
    it "Invalid ID" $ testQueryParams
      (HashMap.singleton "id" "Not a valid ID")
      [r| query humanQuery($id: String!) {
            human(id: $id) {
              name
            }
          }
      |] $ Aeson.object ["data" .= Aeson.object ["human" .= Aeson.Null]]
    it "Luke aliased" $ testQuery
      [r| query FetchLukeAliased {
            luke: human(id: "1000") {
              name
            }
          }
      |]
      $ Aeson.object [ "data" .= Aeson.object [
       "luke" .= Aeson.object [lukeName]
      ]]
    it "R2-D2 ID and friends aliased" $ testQuery
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
      $ Aeson.object [ "data" .= Aeson.object [
        "hero" .= Aeson.object [
            "id" .= ("2001" :: Text)
          , r2d2Name
          , "friends" .=
              [ Aeson.object ["friendName" .= ("Luke Skywalker" :: Text)]
              , Aeson.object ["friendName" .= ("Han Solo" :: Text)]
              , Aeson.object ["friendName" .= ("Leia Organa" :: Text)]
              ]
          ]
      ]]
    it "Luke and Leia aliased" $ testQuery
      [r| query FetchLukeAndLeiaAliased {
            luke: human(id: "1000") {
              name
            }
            leia: human(id: "1003") {
              name
            }
          }
      |]
      $ Aeson.object [ "data" .= Aeson.object
        [ "luke" .= Aeson.object [lukeName]
        , "leia" .= Aeson.object [leiaName]
      ]]

    describe "Fragments for complex queries" $ do
      it "Aliases to query for duplicate content" $ testQuery
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
        $ Aeson.object [ "data" .= Aeson.object [
          "luke" .= Aeson.object [lukeName, tatooine]
        , "leia" .= Aeson.object [leiaName, alderaan]
        ]]
      it "Fragment for duplicate content" $ testQuery
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
        $ Aeson.object [ "data" .= Aeson.object [
          "luke" .= Aeson.object [lukeName, tatooine]
        , "leia" .= Aeson.object [leiaName, alderaan]
        ]]

    describe "__typename" $ do
      it "R2D2 is a Droid" $ testQuery
        [r| query CheckTypeOfR2 {
              hero {
                __typename
                name
              }
            }
          |]
        $ Aeson.object ["data" .= Aeson.object [
            "hero" .= Aeson.object
                [ "__typename" .= ("Droid" :: Text)
                , r2d2Name
                ]
        ]]
      it "Luke is a human" $ testQuery
        [r| query CheckTypeOfLuke {
              hero(episode: EMPIRE) {
                __typename
                name
              }
            }
          |]
        $ Aeson.object ["data" .= Aeson.object [
            "hero" .= Aeson.object
                [ "__typename" .= ("Human" :: Text)
                , lukeName
                ]
        ]]

    describe "Errors in resolvers" $ do
        it "error on secretBackstory" $ testQuery
          [r|
            query HeroNameQuery {
              hero {
                name
                  secretBackstory
              }
            }
          |]
          $ Aeson.object
              [ "data" .= Aeson.object
                  [ "hero" .= Aeson.object
                      [ "name" .= ("R2-D2" :: Text)
                      , "secretBackstory" .= Aeson.Null
                      ]
                  ]
              , "errors" .=
                  [ Aeson.object
                      ["message" .= ("secretBackstory is secret." :: Text)]
                  ]
              ]
        it "Error in a list" $ testQuery
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
          $ Aeson.object ["data" .= Aeson.object
            [ "hero" .= Aeson.object
                [ "name" .= ("R2-D2" :: Text)
                , "friends" .=
                    [ Aeson.object
                        [ "name" .= ("Luke Skywalker" :: Text)
                        , "secretBackstory" .= Aeson.Null
                        ]
                    , Aeson.object
                        [ "name" .= ("Han Solo" :: Text)
                        , "secretBackstory" .= Aeson.Null
                        ]
                    , Aeson.object
                        [ "name" .= ("Leia Organa" :: Text)
                        , "secretBackstory" .= Aeson.Null
                        ]
                    ]
                ]
            ]
            , "errors" .=
                [ Aeson.object
                    [ "message" .= ("secretBackstory is secret." :: Text)
                    ]
                , Aeson.object
                    [ "message" .= ("secretBackstory is secret." :: Text)
                    ]
                , Aeson.object
                    [ "message" .= ("secretBackstory is secret." :: Text)
                    ]
                ]
            ]
        it "error on secretBackstory with alias" $ testQuery
          [r| query HeroNameQuery {
                mainHero: hero {
                  name
                  story: secretBackstory
                }
              }
            |]
          $ Aeson.object
              [ "data" .= Aeson.object
                  [ "mainHero" .= Aeson.object
                      [ "name" .= ("R2-D2" :: Text)
                      , "story" .= Aeson.Null
                      ]
                  ]
              , "errors" .=
                  [ Aeson.object
                    [ "message" .= ("secretBackstory is secret." :: Text)
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

testQuery :: Text -> Aeson.Value -> Expectation
testQuery q expected = runIdentity (graphql schema q) `shouldBe` expected

testQueryParams :: Aeson.Object -> Text -> Aeson.Value -> Expectation
testQueryParams f q expected =
    runIdentity (graphqlSubs schema Nothing f q) `shouldBe` expected
