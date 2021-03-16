{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.GraphQL.Validate.RulesSpec
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
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldContain)
import Text.Megaparsec (parse, errorBundlePretty)
import Text.RawString.QQ (r)

petSchema :: Schema IO
petSchema = schema queryType Nothing (Just subscriptionType) mempty

queryType :: ObjectType IO
queryType = ObjectType "Query" Nothing [] $ HashMap.fromList
    [ ("dog", dogResolver)
    , ("cat", catResolver)
    , ("findDog", findDogResolver)
    ]
  where
    dogField = Field Nothing (Out.NamedObjectType dogType) mempty
    dogResolver = ValueResolver dogField $ pure Null
    findDogArguments = HashMap.singleton "complex"
        $ In.Argument Nothing (In.NonNullInputObjectType dogDataType) Nothing
    findDogField = Field Nothing (Out.NamedObjectType dogType) findDogArguments
    findDogResolver = ValueResolver findDogField $ pure Null
    catField = Field Nothing (Out.NamedObjectType catType) mempty
    catResolver = ValueResolver catField $ pure Null

catCommandType :: EnumType
catCommandType = EnumType "CatCommand" Nothing $ HashMap.fromList
    [ ("JUMP", EnumValue Nothing)
    ]

catType :: ObjectType IO
catType = ObjectType "Cat" Nothing [petType] $ HashMap.fromList
    [ ("name", nameResolver)
    , ("nickname", nicknameResolver)
    , ("doesKnowCommands", doesKnowCommandsResolver)
    , ("meowVolume", meowVolumeResolver)
    ]
  where
    meowVolumeField = Field Nothing (Out.NamedScalarType int) mempty
    meowVolumeResolver = ValueResolver meowVolumeField $ pure $ Int 3
    doesKnowCommandsType = In.NonNullListType
        $ In.NonNullEnumType catCommandType
    doesKnowCommandsField = Field Nothing (Out.NonNullScalarType boolean)
        $ HashMap.singleton "catCommands"
        $ In.Argument Nothing doesKnowCommandsType Nothing
    doesKnowCommandsResolver = ValueResolver doesKnowCommandsField
        $ pure $ Boolean True

nameResolver :: Resolver IO
nameResolver = ValueResolver nameField $ pure "Name"
  where
    nameField = Field Nothing (Out.NonNullScalarType string) mempty

nicknameResolver :: Resolver IO
nicknameResolver = ValueResolver nicknameField $ pure "Nickname"
  where
    nicknameField = Field Nothing (Out.NamedScalarType string) mempty

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
    petsField =
        Field Nothing (Out.ListType $ Out.NonNullInterfaceType petType) mempty
    petsResolver = ValueResolver petsField $ pure $ List []

validate :: Text -> [Error]
validate queryString =
    case parse AST.document "" queryString of
        Left parseErrors -> error $ errorBundlePretty parseErrors
        Right ast -> toList $ document petSchema specifiedRules ast

spec :: Spec
spec =
    describe "document" $ do
        context "executableDefinitionsRule" $
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
                            "Definition must be OperationDefinition or \
                            \FragmentDefinition."
                        , locations = [AST.Location 9 19]
                        }
                 in validate queryString `shouldContain` [expected]

        context "singleFieldSubscriptionsRule" $ do
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
                            "Subscription \"sub\" must select only one top \
                            \level field."
                        , locations = [AST.Location 2 19]
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
                            "Subscription \"sub\" must select only one top \
                            \level field."
                        , locations = [AST.Location 2 19]
                        }
                 in validate queryString `shouldContain` [expected]

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
                            "Subscription \"sub\" must select only one top \
                            \level field."
                        , locations = [AST.Location 2 19]
                        }
                 in validate queryString `shouldBe` [expected]

        context "loneAnonymousOperationRule" $
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
                            "This anonymous operation must be the only defined \
                            \operation."
                        , locations = [AST.Location 2 19]
                        }
                 in validate queryString `shouldBe` [expected]

        context "uniqueOperationNamesRule" $
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
                            "There can be only one operation named \
                            \\"dogOperation\"."
                        , locations = [AST.Location 2 19, AST.Location 8 19]
                        }
                 in validate queryString `shouldBe` [expected]

        context "uniqueFragmentNamesRule" $
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
                            "There can be only one fragment named \
                            \\"fragmentOne\"."
                        , locations = [AST.Location 8 19, AST.Location 12 19]
                        }
                 in validate queryString `shouldBe` [expected]

        context "fragmentSpreadTargetDefinedRule" $
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
                            "Fragment target \"undefinedFragment\" is \
                            \undefined."
                        , locations = [AST.Location 4 23]
                        }
                 in validate queryString `shouldBe` [expected]

        context "fragmentSpreadTypeExistenceRule" $ do
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
                            "Fragment \"notOnExistingType\" is specified on \
                            \type \"NotInSchema\" which doesn't exist in the \
                            \schema."
                        , locations = [AST.Location 4 23]
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
                            "Inline fragment is specified on type \
                            \\"NotInSchema\" which doesn't exist in the schema."
                        , locations = [AST.Location 3 21]
                        }
                 in validate queryString `shouldBe` [expected]

        context "fragmentsOnCompositeTypesRule" $ do
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
                        , locations = [AST.Location 7 19]
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
                        , locations = [AST.Location 3 21]
                        }
                in validate queryString `shouldContain` [expected]

        context "noUnusedFragmentsRule" $
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
                        , locations = [AST.Location 2 19]
                        }
                 in validate queryString `shouldBe` [expected]

        context "noFragmentCyclesRule" $
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
                            "Cannot spread fragment \"barkVolumeFragment\" \
                            \within itself (via barkVolumeFragment -> \
                            \nameFragment -> barkVolumeFragment)."
                        , locations = [AST.Location 11 19]
                        }
                    error2 = Error
                        { message =
                            "Cannot spread fragment \"nameFragment\" within \
                            \itself (via nameFragment -> barkVolumeFragment -> \
                            \nameFragment)."
                        , locations = [AST.Location 7 19]
                        }
                 in validate queryString `shouldBe` [error1, error2]

        context "uniqueArgumentNamesRule" $
            it "rejects duplicate field arguments" $
                let queryString = [r|
                  {
                    dog {
                      isHousetrained(atOtherHomes: true, atOtherHomes: true)
                    }
                  }
                |]
                    expected = Error
                        { message =
                            "There can be only one argument named \
                            \\"atOtherHomes\"."
                        , locations = [AST.Location 4 38, AST.Location 4 58]
                        }
                 in validate queryString `shouldBe` [expected]

        context "uniqueDirectiveNamesRule" $
            it "rejects more than one directive per location" $
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
                        , locations = [AST.Location 3 25, AST.Location 3 41]
                        }
                 in validate queryString `shouldBe` [expected]

        context "uniqueVariableNamesRule" $
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
                            "There can be only one variable named \
                            \\"atOtherHomes\"."
                        , locations = [AST.Location 2 43, AST.Location 2 67]
                        }
                 in validate queryString `shouldBe` [expected]

        context "variablesAreInputTypesRule" $
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
                            "Variable \"$dog\" cannot be non-input type \
                            \\"Dog\"."
                        , locations = [AST.Location 2 38]
                        }
                 in validate queryString `shouldContain` [expected]

        context "noUndefinedVariablesRule" $
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
                        , locations = [AST.Location 9 50]
                        }
                 in validate queryString `shouldBe` [expected]

        context "noUnusedVariablesRule" $
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
                            "Variable \"$atOtherHomes\" is never used in \
                            \operation \"variableUnused\"."
                        , locations = [AST.Location 2 40]
                        }
                 in validate queryString `shouldBe` [expected]

        context "uniqueInputFieldNamesRule" $
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
                        , locations = [AST.Location 3 40, AST.Location 3 54]
                        }
                 in validate queryString `shouldBe` [expected]

        context "fieldsOnCorrectTypeRule" $
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
                        , locations = [AST.Location 4 23]
                        }
                 in validate queryString `shouldBe` [expected]

        context "scalarLeafsRule" $
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
                            "Field \"barkVolume\" must not have a selection \
                            \since type \"Int\" has no subfields."
                        , locations = [AST.Location 4 23]
                        }
                 in validate queryString `shouldBe` [expected]

        context "knownArgumentNamesRule" $ do
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
                        , locations = [AST.Location 4 39]
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
                            "Unknown argument \"unless\" on directive \
                            \\"@include\"."
                        , locations = [AST.Location 4 67]
                        }
                 in validate queryString `shouldBe` [expected]

        context "knownDirectiveNamesRule" $
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
                        , locations = [AST.Location 4 58]
                        }
                 in validate queryString `shouldBe` [expected]

        context "knownInputFieldNamesRule" $
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
                        , locations = [AST.Location 3 40]
                        }
                 in validate queryString `shouldBe` [expected]

        context "directivesInValidLocationsRule" $
            it "rejects directives in invalid locations" $
                let queryString = [r|
                  query @skip(if: $foo) {
                    dog {
                      name
                    }
                  }
                |]
                    expected = Error
                        { message =
                            "Directive \"@skip\" may not be used on QUERY."
                        , locations = [AST.Location 2 25]
                        }
                 in validate queryString `shouldBe` [expected]

        context "overlappingFieldsCanBeMergedRule" $ do
            it "fails to merge fields of mismatching types" $
                let queryString = [r|
                  {
                    dog {
                      name: nickname
                      name
                    }
                  }
                |]
                    expected = Error
                        { message =
                            "Fields \"name\" conflict because \"nickname\" and \
                            \\"name\" are different fields. Use different \
                            \aliases on the fields to fetch both if this was \
                            \intentional."
                        , locations = [AST.Location 4 23, AST.Location 5 23]
                        }
                 in validate queryString `shouldBe` [expected]

            it "fails if the arguments of the same field don't match" $
                let queryString = [r|
                  {
                    dog {
                      doesKnowCommand(dogCommand: SIT)
                      doesKnowCommand(dogCommand: HEEL)
                    }
                  }
                |]
                    expected = Error
                        { message =
                            "Fields \"doesKnowCommand\" conflict because they \
                            \have different arguments. Use different aliases \
                            \on the fields to fetch both if this was \
                            \intentional."
                        , locations = [AST.Location 4 23, AST.Location 5 23]
                        }
                 in validate queryString `shouldBe` [expected]

            it "fails to merge same-named field and alias" $
                let queryString = [r|
                  {
                    dog {
                      doesKnowCommand(dogCommand: SIT)
                      doesKnowCommand: isHousetrained(atOtherHomes: true)
                    }
                  }
                |]
                    expected = Error
                        { message =
                            "Fields \"doesKnowCommand\" conflict because \
                            \\"doesKnowCommand\" and \"isHousetrained\" are \
                            \different fields. Use different aliases on the \
                            \fields to fetch both if this was intentional."
                        , locations = [AST.Location 4 23, AST.Location 5 23]
                        }
                 in validate queryString `shouldBe` [expected]

            it "looks for fields after a successfully merged field pair" $
                let queryString = [r|
                  {
                    dog {
                      name
                      doesKnowCommand(dogCommand: SIT)
                    }
                    dog {
                      name
                      doesKnowCommand: isHousetrained(atOtherHomes: true)
                    }
                  }
                |]
                    expected = Error
                        { message =
                            "Fields \"doesKnowCommand\" conflict because \
                            \\"doesKnowCommand\" and \"isHousetrained\" are \
                            \different fields. Use different aliases on the \
                            \fields to fetch both if this was intentional."
                        , locations = [AST.Location 5 23, AST.Location 9 23]
                        }
                 in validate queryString `shouldBe` [expected]

        context "possibleFragmentSpreadsRule" $ do
            it "rejects object inline spreads outside object scope" $
                let queryString = [r|
                  {
                    dog {
                      ... on Cat {
                        meowVolume
                      }
                    }
                  }
                |]
                    expected = Error
                        { message =
                            "Fragment cannot be spread here as objects of type \
                            \\"Dog\" can never be of type \"Cat\"."
                        , locations = [AST.Location 4 23]
                        }
                 in validate queryString `shouldBe` [expected]

            it "rejects object named spreads outside object scope" $
                let queryString = [r|
                  {
                    dog {
                      ... catInDogFragmentInvalid
                    }
                  }

                  fragment catInDogFragmentInvalid on Cat {
                    meowVolume
                  }
                |]
                    expected = Error
                        { message =
                            "Fragment \"catInDogFragmentInvalid\" cannot be \
                            \spread here as objects of type \"Dog\" can never \
                            \be of type \"Cat\"."
                        , locations = [AST.Location 4 23]
                        }
                 in validate queryString `shouldBe` [expected]

        context "providedRequiredInputFieldsRule" $
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
                            "Input field \"name\" of type \"DogData\" is \
                            \required, but it was not provided."
                        , locations = [AST.Location 3 38]
                        }
                 in validate queryString `shouldBe` [expected]

        context "providedRequiredArgumentsRule" $
            it "checks for (non-)nullable arguments" $
                let queryString = [r|
                  {
                    dog {
                      doesKnowCommand(dogCommand: null)
                    }
                  }
                |]
                    expected = Error
                        { message =
                            "Field \"doesKnowCommand\" argument \"dogCommand\" \
                            \of type \"DogCommand\" is required, but it was \
                            \not provided."
                        , locations = [AST.Location 4 23]
                        }
                 in validate queryString `shouldBe` [expected]

        context "variablesInAllowedPositionRule" $ do
            it "rejects wrongly typed variable arguments" $
                let queryString = [r|
                  query dogCommandArgQuery($dogCommandArg: DogCommand) {
                    dog {
                      doesKnowCommand(dogCommand: $dogCommandArg)
                    }
                  }
                |]
                    expected = Error
                        { message =
                            "Variable \"$dogCommandArg\" of type \
                            \\"DogCommand\" used in position expecting type \
                            \\"!DogCommand\"."
                        , locations = [AST.Location 2 44]
                        }
                 in validate queryString `shouldBe` [expected]

            it "rejects wrongly typed variable arguments" $
                let queryString = [r|
                  query intCannotGoIntoBoolean($intArg: Int) {
                    dog {
                      isHousetrained(atOtherHomes: $intArg)
                    }
                  }
                |]
                    expected = Error
                        { message =
                            "Variable \"$intArg\" of type \"Int\" used in \
                            \position expecting type \"Boolean\"."
                        , locations = [AST.Location 2 48]
                        }
                 in validate queryString `shouldBe` [expected]

        context "valuesOfCorrectTypeRule" $ do
            it "rejects values of incorrect types" $
                let queryString = [r|
                  {
                    dog {
                      isHousetrained(atOtherHomes: 3)
                    }
                  }
                |]
                    expected = Error
                        { message =
                            "Value 3 cannot be coerced to type \"Boolean\"."
                        , locations = [AST.Location 4 52]
                        }
                 in validate queryString `shouldBe` [expected]

            it "uses the location of a single list value" $
                let queryString = [r|
                  {
                    cat {
                      doesKnowCommands(catCommands: [3])
                    }
                  }
                |]
                    expected = Error
                        { message =
                            "Value 3 cannot be coerced to type \"!CatCommand\"."
                        , locations = [AST.Location 4 54]
                        }
                 in validate queryString `shouldBe` [expected]
