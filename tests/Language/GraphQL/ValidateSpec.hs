{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.GraphQL.ValidateSpec
    ( spec
    ) where

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Language.GraphQL.AST as AST
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Validate
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

schema :: Schema IO
schema = Schema
    { query = queryType
    , mutation = Nothing
    , subscription = Nothing
    } 

queryType :: ObjectType IO
queryType = ObjectType "Query" Nothing []
    $ HashMap.singleton "dog" dogResolver
  where
    dogField = Field Nothing (Out.NamedObjectType dogType) mempty
    dogResolver = ValueResolver dogField $ pure Null

dogCommandType :: EnumType
dogCommandType = EnumType "DogCommand" Nothing $ HashMap.fromList
    [ ("SIT", EnumValue Nothing)
    , ("DOWN", EnumValue Nothing)
    , ("HEEL", EnumValue Nothing)
    ]

dogType :: ObjectType IO
dogType = ObjectType "Dog" Nothing [petType] $ HashMap.fromList
    [ ("name", nameResolver)
    , ("nickname", nicknameResolver)
    , ("barkVolume", barkVolumeResolver)
    , ("doesKnowCommand", doesKnowCommandResolver)
    , ("isHousetrained", isHousetrainedResolver)
    , ("owner", ownerResolver)
    ]
  where
    nameField = Field Nothing (Out.NonNullScalarType string) mempty
    nameResolver = ValueResolver nameField $ pure "Name"
    nicknameField = Field Nothing (Out.NamedScalarType string) mempty
    nicknameResolver = ValueResolver nicknameField $ pure "Nickname"
    barkVolumeField = Field Nothing (Out.NamedScalarType int) mempty
    barkVolumeResolver = ValueResolver barkVolumeField $ pure $ Int 3
    doesKnowCommandField = Field Nothing (Out.NonNullScalarType boolean)
        $ HashMap.singleton "dogCommand"
        $ In.Argument Nothing (In.NonNullEnumType dogCommandType) Nothing
    doesKnowCommandResolver = ValueResolver doesKnowCommandField
        $ pure $ Boolean True
    isHousetrainedField = Field Nothing (Out.NonNullScalarType boolean)
        $ HashMap.singleton "atOtherHomes"
        $ In.Argument Nothing (In.NamedScalarType boolean) Nothing
    isHousetrainedResolver = ValueResolver isHousetrainedField
        $ pure $ Boolean True
    ownerField = Field Nothing (Out.NamedObjectType humanType) mempty
    ownerResolver = ValueResolver ownerField $ pure Null

sentientType :: InterfaceType IO
sentientType = InterfaceType "Sentient" Nothing []
    $ HashMap.singleton "name"
    $ Field Nothing (Out.NonNullScalarType string) mempty

petType :: InterfaceType IO
petType = InterfaceType "Pet" Nothing []
    $ HashMap.singleton "name"
    $ Field Nothing (Out.NonNullScalarType string) mempty
{-
alienType :: ObjectType IO
alienType = ObjectType "Alien" Nothing [sentientType] $ HashMap.fromList
    [ ("name", nameResolver)
    , ("homePlanet", homePlanetResolver)
    ]
  where
    nameField = Field Nothing (Out.NonNullScalarType string) mempty
    nameResolver = ValueResolver nameField $ pure "Name"
    homePlanetField =
        Field Nothing (Out.NamedScalarType string) mempty
    homePlanetResolver = ValueResolver homePlanetField $ pure "Home planet"
-}
humanType :: ObjectType IO
humanType = ObjectType "Human" Nothing [sentientType] $ HashMap.fromList
    [ ("name", nameResolver)
    , ("pets", petsResolver)
    ]
  where
    nameField = Field Nothing (Out.NonNullScalarType string) mempty
    nameResolver = ValueResolver nameField $ pure "Name"
    petsField =
        Field Nothing (Out.ListType $ Out.NonNullInterfaceType petType) mempty
    petsResolver = ValueResolver petsField $ pure $ List []
{-
catCommandType :: EnumType
catCommandType = EnumType "CatCommand" Nothing $ HashMap.fromList
    [ ("JUMP", EnumValue Nothing)
    ]

catType :: ObjectType IO
catType = ObjectType "Cat" Nothing [petType] $ HashMap.fromList
    [ ("name", nameResolver)
    , ("nickname", nicknameResolver)
    , ("doesKnowCommand", doesKnowCommandResolver)
    , ("meowVolume", meowVolumeResolver)
    ]
  where
    nameField = Field Nothing (Out.NonNullScalarType string) mempty
    nameResolver = ValueResolver nameField $ pure "Name"
    nicknameField = Field Nothing (Out.NamedScalarType string) mempty
    nicknameResolver = ValueResolver nicknameField $ pure "Nickname"
    doesKnowCommandField = Field Nothing (Out.NonNullScalarType boolean)
        $ HashMap.singleton "catCommand"
        $ In.Argument Nothing (In.NonNullEnumType catCommandType) Nothing
    doesKnowCommandResolver = ValueResolver doesKnowCommandField
        $ pure $ Boolean True
    meowVolumeField = Field Nothing (Out.NamedScalarType int) mempty
    meowVolumeResolver = ValueResolver meowVolumeField $ pure $ Int 2

catOrDogType :: UnionType IO
catOrDogType = UnionType "CatOrDog" Nothing [catType, dogType]

dogOrHumanType :: UnionType IO
dogOrHumanType = UnionType "DogOrHuman" Nothing [dogType, humanType]

humanOrAlienType :: UnionType IO
humanOrAlienType = UnionType "HumanOrAlien" Nothing [humanType, alienType]
-}
validate :: Text -> Seq Error
validate queryString =
    case parse AST.document "" queryString of
        Left _ -> Seq.empty
        Right ast -> document schema specifiedRules ast

spec :: Spec
spec =
    describe "document" $ do
        it "rejects type definitions" $
            let queryString = [r|
              query getDogName {
                dog {
                  name
                  color
                }
              }

              extend type Dog {
                color: String
              }
            |]
                expected = Error
                    { message =
                        "Definition must be OperationDefinition or FragmentDefinition."
                    , locations = [AST.Location 9 15]
                    , path = []
                    }
             in validate queryString `shouldBe` Seq.singleton expected

        it "rejects multiple subscription root fields" $
            let queryString = [r|
              subscription sub {
                newMessage {
                  body
                  sender
                }
                disallowedSecondRootField
              }
            |]
                expected = Error
                    { message =
                        "Subscription sub must select only one top level field."
                    , locations = [AST.Location 2 15]
                    , path = []
                    }
             in validate queryString `shouldBe` Seq.singleton expected

        it "rejects multiple subscription root fields coming from a fragment" $
            let queryString = [r|
              subscription sub {
                ...multipleSubscriptions
              }

              fragment multipleSubscriptions on Subscription {
                newMessage {
                  body
                  sender
                }
                disallowedSecondRootField
              }
            |]
                expected = Error
                    { message =
                        "Subscription sub must select only one top level field."
                    , locations = [AST.Location 2 15]
                    , path = []
                    }
             in validate queryString `shouldBe` Seq.singleton expected

        it "rejects multiple anonymous operations" $
            let queryString = [r|
              {
                dog {
                  name
                }
              }

              query getName {
                dog {
                  owner {
                    name
                  }
                }
              }
            |]
                expected = Error
                    { message =
                        "This anonymous operation must be the only defined operation."
                    , locations = [AST.Location 2 15]
                    , path = []
                    }
             in validate queryString `shouldBe` Seq.singleton expected
