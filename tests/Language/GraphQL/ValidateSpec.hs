{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.GraphQL.ValidateSpec
    ( spec
    ) where

import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Language.GraphQL.AST as AST
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Validate
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

petSchema :: Schema IO
petSchema = schema queryType Nothing (Just subscriptionType) mempty

queryType :: ObjectType IO
queryType = ObjectType "Query" Nothing [] $ HashMap.fromList
    [ ("dog", dogResolver)
    , ("findDog", findDogResolver)
    ]
  where
    dogField = Field Nothing (Out.NamedObjectType dogType) mempty
    dogResolver = ValueResolver dogField $ pure Null
    findDogArguments = HashMap.singleton "complex"
        $ In.Argument Nothing (In.NonNullInputObjectType dogDataType) Nothing
    findDogField = Field Nothing (Out.NamedObjectType dogType) findDogArguments
    findDogResolver = ValueResolver findDogField $ pure Null

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

dogDataType :: InputObjectType
dogDataType = InputObjectType "DogData" Nothing
    $ HashMap.singleton "name" nameInputField
  where
    nameInputField = InputField Nothing (In.NonNullScalarType string) Nothing

sentientType :: InterfaceType IO
sentientType = InterfaceType "Sentient" Nothing []
    $ HashMap.singleton "name"
    $ Field Nothing (Out.NonNullScalarType string) mempty

petType :: InterfaceType IO
petType = InterfaceType "Pet" Nothing []
    $ HashMap.singleton "name"
    $ Field Nothing (Out.NonNullScalarType string) mempty

subscriptionType :: ObjectType IO
subscriptionType = ObjectType "Subscription" Nothing [] $ HashMap.fromList
    [ ("newMessage", newMessageResolver)
    , ("disallowedSecondRootField", newMessageResolver)
    ]
  where
    newMessageField = Field Nothing (Out.NonNullObjectType messageType) mempty
    newMessageResolver = ValueResolver newMessageField
        $ pure $ Object HashMap.empty

messageType :: ObjectType IO
messageType = ObjectType "Message" Nothing [] $ HashMap.fromList
    [ ("sender", senderResolver)
    , ("body", bodyResolver)
    ]
  where
    senderField = Field Nothing (Out.NonNullScalarType string) mempty
    senderResolver = ValueResolver senderField $ pure "Sender"
    bodyField = Field Nothing (Out.NonNullScalarType string) mempty
    bodyResolver = ValueResolver bodyField $ pure "Message body."

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
catOrDogType :: UnionType IO
catOrDogType = UnionType "CatOrDog" Nothing [catType, dogType]
-}
validate :: Text -> [Error]
validate queryString =
    case parse AST.document "" queryString of
        Left _ -> []
        Right ast -> toList $ document petSchema specifiedRules ast

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
                    }
               in validate queryString `shouldContain` [expected]

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
                        "Subscription \"sub\" must select only one top level \
                        \field."
                    , locations = [AST.Location 2 15]
                    }
             in validate queryString `shouldContain` [expected]

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
                        "Subscription \"sub\" must select only one top level \
                        \field."
                    , locations = [AST.Location 2 15]
                    }
             in validate queryString `shouldContain` [expected]

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
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects operations with the same name" $
            let queryString = [r|
              query dogOperation {
                dog {
                  name
                }
              }

              mutation dogOperation {
                mutateDog {
                  id
                }
            }
            |]
                expected = Error
                    { message =
                        "There can be only one operation named \"dogOperation\"."
                    , locations = [AST.Location 2 15, AST.Location 8 15]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects fragments with the same name" $
            let queryString = [r|
              {
                dog {
                  ...fragmentOne
                }
              }

              fragment fragmentOne on Dog {
                name
              }

              fragment fragmentOne on Dog {
                owner {
                  name
                }
              }
            |]
                expected = Error
                    { message =
                        "There can be only one fragment named \"fragmentOne\"."
                    , locations = [AST.Location 8 15, AST.Location 12 15]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects the fragment spread without a target" $
            let queryString = [r|
              {
                dog {
                  ...undefinedFragment
                }
              }
            |]
                expected = Error
                    { message =
                        "Fragment target \"undefinedFragment\" is undefined."
                    , locations = [AST.Location 4 19]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects fragment spreads without an unknown target type" $
            let queryString = [r|
              {
                dog {
                  ...notOnExistingType
                }
              }
              fragment notOnExistingType on NotInSchema {
                name
              }
            |]
                expected = Error
                    { message =
                        "Fragment \"notOnExistingType\" is specified on type \
                        \\"NotInSchema\" which doesn't exist in the schema."
                    , locations = [AST.Location 4 19]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects inline fragments without a target" $
            let queryString = [r|
              {
                ... on NotInSchema {
                  name
                }
              }
            |]
                expected = Error
                    { message =
                        "Inline fragment is specified on type \"NotInSchema\" \
                        \which doesn't exist in the schema."
                    , locations = [AST.Location 3 17]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects fragments on scalar types" $
            let queryString = [r|
              {
                dog {
                  ...fragOnScalar
                }
              }
              fragment fragOnScalar on Int {
                name
              }
            |]
                expected = Error
                    { message =
                        "Fragment cannot condition on non composite type \
                        \\"Int\"."
                    , locations = [AST.Location 7 15]
                    }
             in validate queryString `shouldContain` [expected]

        it "rejects inline fragments on scalar types" $
            let queryString = [r|
              {
                ... on Boolean {
                  name
                }
              }
            |]
                expected = Error
                    { message =
                        "Fragment cannot condition on non composite type \
                        \\"Boolean\"."
                    , locations = [AST.Location 3 17]
                    }
             in validate queryString `shouldContain` [expected]

        it "rejects unused fragments" $
            let queryString = [r|
              fragment nameFragment on Dog { # unused
                name
              }

              {
                dog {
                  name
                }
              }
            |]
                expected = Error
                    { message =
                        "Fragment \"nameFragment\" is never used."
                    , locations = [AST.Location 2 15]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects spreads that form cycles" $
            let queryString = [r|
              {
                dog {
                  ...nameFragment
                }
              }
              fragment nameFragment on Dog {
                name
                ...barkVolumeFragment
              }
              fragment barkVolumeFragment on Dog {
                barkVolume
                ...nameFragment
              }
            |]
                error1 = Error
                    { message =
                        "Cannot spread fragment \"barkVolumeFragment\" within \
                        \itself (via barkVolumeFragment -> nameFragment -> \
                        \barkVolumeFragment)."
                    , locations = [AST.Location 11 15]
                    }
                error2 = Error
                    { message =
                        "Cannot spread fragment \"nameFragment\" within itself \
                        \(via nameFragment -> barkVolumeFragment -> \
                        \nameFragment)."
                    , locations = [AST.Location 7 15]
                    }
             in validate queryString `shouldBe` [error1, error2]

        it "rejects duplicate field arguments" $ do
            let queryString = [r|
              {
                dog {
                  isHousetrained(atOtherHomes: true, atOtherHomes: true)
                }
              }
            |]
                expected = Error
                    { message =
                        "There can be only one argument named \"atOtherHomes\"."
                    , locations = [AST.Location 4 34, AST.Location 4 54]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects more than one directive per location" $ do
            let queryString = [r|
              query ($foo: Boolean = true, $bar: Boolean = false) {
                dog @skip(if: $foo) @skip(if: $bar) {
                  name
                }
              }
            |]
                expected = Error
                    { message =
                        "There can be only one directive named \"skip\"."
                    , locations = [AST.Location 3 21, AST.Location 3 37]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects duplicate variables" $
            let queryString = [r|
              query houseTrainedQuery($atOtherHomes: Boolean, $atOtherHomes: Boolean) {
                dog {
                  isHousetrained(atOtherHomes: $atOtherHomes)
                }
              }
            |]
                expected = Error
                    { message =
                        "There can be only one variable named \"atOtherHomes\"."
                    , locations = [AST.Location 2 39, AST.Location 2 63]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects non-input types as variables" $
            let queryString = [r|
              query takesDogBang($dog: Dog!) {
                dog {
                  isHousetrained(atOtherHomes: $dog)
                }
              }
            |]
                expected = Error
                    { message =
                        "Variable \"$dog\" cannot be non-input type \"Dog\"."
                    , locations = [AST.Location 2 34]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects undefined variables" $
            let queryString = [r|
              query variableIsNotDefinedUsedInSingleFragment {
                dog {
                  ...isHousetrainedFragment
                }
              }

              fragment isHousetrainedFragment on Dog {
                isHousetrained(atOtherHomes: $atOtherHomes)
              }
            |]
                expected = Error
                    { message =
                        "Variable \"$atOtherHomes\" is not defined by \
                        \operation \
                        \\"variableIsNotDefinedUsedInSingleFragment\"."
                    , locations = [AST.Location 9 46]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects unused variables" $
            let queryString = [r|
              query variableUnused($atOtherHomes: Boolean) {
                dog {
                  isHousetrained
                }
              }
            |]
                expected = Error
                    { message =
                        "Variable \"$atOtherHomes\" is never used in operation \
                        \\"variableUnused\"."
                    , locations = [AST.Location 2 36]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects duplicate fields in input objects" $
            let queryString = [r|
              {
                findDog(complex: { name: "Fido", name: "Jack" }) {
                  name
                }
              }
            |]
                expected = Error
                    { message =
                        "There can be only one input field named \"name\"."
                    , locations = [AST.Location 3 36, AST.Location 3 50]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects undefined fields" $
            let queryString = [r|
              {
                dog {
                  meowVolume
                }
              }
            |]
                expected = Error
                    { message =
                        "Cannot query field \"meowVolume\" on type \"Dog\"."
                    , locations = [AST.Location 4 19]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects scalar fields with not empty selection set" $
            let queryString = [r|
              {
                dog {
                  barkVolume {
                    sinceWhen
                  }
                }
              }
            |]
                expected = Error
                    { message =
                        "Field \"barkVolume\" must not have a selection since \
                        \type \"Int\" has no subfields."
                    , locations = [AST.Location 4 19]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects field arguments missing in the type" $
            let queryString = [r|
              {
                dog {
                  doesKnowCommand(command: CLEAN_UP_HOUSE, dogCommand: SIT)
                }
              }
            |]
                expected = Error
                    { message =
                        "Unknown argument \"command\" on field \
                        \\"Dog.doesKnowCommand\"."
                    , locations = [AST.Location 4 35]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects directive arguments missing in the definition" $
            let queryString = [r|
              {
                dog {
                  isHousetrained(atOtherHomes: true) @include(unless: false, if: true)
                }
              }
            |]
                expected = Error
                    { message =
                        "Unknown argument \"unless\" on directive \"@include\"."
                    , locations = [AST.Location 4 63]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects undefined directives" $
            let queryString = [r|
              {
                dog {
                  isHousetrained(atOtherHomes: true) @ignore(if: true)
                }
              }
            |]
                expected = Error
                    { message = "Unknown directive \"@ignore\"."
                    , locations = [AST.Location 4 54]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects undefined input object fields" $
            let queryString = [r|
              {
                findDog(complex: { favoriteCookieFlavor: "Bacon", name: "Jack" }) {
                  name
                }
              }
            |]
                expected = Error
                    { message =
                        "Field \"favoriteCookieFlavor\" is not defined \
                        \by type \"DogData\"."
                    , locations = [AST.Location 3 36]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects directives in invalid locations" $
            let queryString = [r|
              query @skip(if: $foo) {
                dog {
                  name
                }
              }
            |]
                expected = Error
                    { message = "Directive \"@skip\" may not be used on QUERY."
                    , locations = [AST.Location 2 21]
                    }
             in validate queryString `shouldBe` [expected]

        it "rejects missing required input fields" $
            let queryString = [r|
              {
                findDog(complex: { name: null }) {
                  name
                }
              }
            |]
                expected = Error
                    { message =
                        "Input field \"name\" of type \"DogData\" is required, \
                        \but it was not provided."
                    , locations = [AST.Location 3 34]
                    }
             in validate queryString `shouldBe` [expected]

        it "finds corresponding subscription fragment" $
            let queryString = [r|
              subscription sub {
                ...anotherSubscription
                ...multipleSubscriptions
              }
              fragment multipleSubscriptions on Subscription {
                newMessage {
                  body
                }
                disallowedSecondRootField {
                  sender
                }
              }
              fragment anotherSubscription on Subscription {
                newMessage {
                  body
                  sender
                }
              }
            |]
                expected = Error
                    { message =
                        "Subscription \"sub\" must select only one top level \
                        \field."
                    , locations = [AST.Location 2 15]
                    }
             in validate queryString `shouldBe` [expected]
