{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.Execute.CoerceSpec
    ( spec
    ) where

import Data.Aeson as Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isNothing)
import Data.Scientific (scientific)
import Language.GraphQL.Execute.Coerce
import Language.GraphQL.Type.Definition
import qualified Language.GraphQL.Type.In as In
import Prelude hiding (id)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

direction :: EnumType
direction = EnumType "Direction" Nothing  $ HashMap.fromList
    [ ("NORTH", EnumValue Nothing)
    , ("EAST", EnumValue Nothing)
    , ("SOUTH", EnumValue Nothing)
    , ("WEST", EnumValue Nothing)
    ]

singletonInputObject :: In.Type
singletonInputObject = In.NamedInputObjectType type'
  where
    type' = In.InputObjectType "ObjectName" Nothing inputFields
    inputFields = HashMap.singleton "field" field
    field = In.InputField Nothing (In.NamedScalarType string) Nothing

spec :: Spec
spec = do
    describe "VariableValue Aeson" $ do
        it "coerces strings" $
            let expected = Just (String "asdf")
                actual = coerceVariableValue
                    (In.NamedScalarType string) (Aeson.String "asdf")
             in actual `shouldBe` expected
        it "coerces non-null strings" $
            let expected = Just (String "asdf")
                actual = coerceVariableValue
                    (In.NonNullScalarType string) (Aeson.String "asdf")
             in actual `shouldBe` expected
        it "coerces booleans" $
            let expected = Just (Boolean True)
                actual = coerceVariableValue
                    (In.NamedScalarType boolean) (Aeson.Bool True)
             in actual `shouldBe` expected
        it "coerces zero to an integer" $
            let expected = Just (Int 0)
                actual = coerceVariableValue
                    (In.NamedScalarType int) (Aeson.Number 0)
             in actual `shouldBe` expected
        it "rejects fractional if an integer is expected" $
            let actual = coerceVariableValue
                    (In.NamedScalarType int) (Aeson.Number $ scientific 14 (-1))
             in actual `shouldSatisfy` isNothing
        it "coerces float numbers" $
            let expected = Just (Float 1.4)
                actual = coerceVariableValue
                    (In.NamedScalarType float) (Aeson.Number $ scientific 14 (-1))
             in actual `shouldBe` expected
        it "coerces IDs" $
            let expected = Just (String "1234")
                actual = coerceVariableValue
                    (In.NamedScalarType id) (Aeson.String "1234")
             in actual `shouldBe` expected
        it "coerces input objects" $
            let actual = coerceVariableValue singletonInputObject
                    $ Aeson.object ["field" .= ("asdf" :: Aeson.Value)]
                expected = Just $ Object $ HashMap.singleton "field" "asdf"
             in actual `shouldBe` expected
        it "skips the field if it is missing in the variables" $
            let actual = coerceVariableValue
                    singletonInputObject Aeson.emptyObject
                expected = Just $ Object HashMap.empty
             in actual `shouldBe` expected
        it "fails if input object value contains extra fields" $
            let actual = coerceVariableValue singletonInputObject
                    $ Aeson.object variableFields
                variableFields =
                    [ "field" .= ("asdf" :: Aeson.Value)
                    , "extra" .= ("qwer" :: Aeson.Value)
                    ]
             in actual `shouldSatisfy` isNothing
        it "preserves null" $
            let actual = coerceVariableValue (In.NamedScalarType id) Aeson.Null
             in actual `shouldBe` Just Null
        it "preserves list order" $
            let list = Aeson.toJSONList ["asdf" :: Aeson.Value, "qwer"]
                listType = (In.ListType $ In.NamedScalarType string)
                actual = coerceVariableValue listType list
                expected = Just $ List [String "asdf", String "qwer"]
             in actual `shouldBe` expected

    describe "coerceInputLiterals" $ do
        it "coerces enums" $
            let expected = Just (Enum "NORTH")
                actual = coerceInputLiteral
                    (In.NamedEnumType direction) (Enum "NORTH")
             in actual `shouldBe` expected
        it "fails with non-existing enum value" $
            let actual = coerceInputLiteral
                    (In.NamedEnumType direction) (Enum "NORTH_EAST")
             in actual `shouldSatisfy` isNothing
        it "coerces integers to IDs" $
            let expected = Just (String "1234")
                actual = coerceInputLiteral (In.NamedScalarType id) (Int 1234)
             in actual `shouldBe` expected
