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
import Language.GraphQL.AST.Core
import Language.GraphQL.Execute.Coerce
import Language.GraphQL.Type.Definition
import Prelude hiding (id)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

singletonInputObject :: InputType
singletonInputObject = ObjectInputType type'
  where
    type' = InputObjectType "ObjectName" Nothing inputFields
    inputFields = HashMap.singleton "field" field
    field = InputField Nothing (ScalarInputType string) Nothing

spec :: Spec
spec =
    describe "ToGraphQL Aeson" $ do
        it "coerces strings" $
            let expected = Just (String "asdf")
                actual = coerceVariableValue
                    (ScalarInputType string) (Aeson.String "asdf")
             in actual `shouldBe` expected
        it "coerces non-null strings" $
            let expected = Just (String "asdf")
                actual = coerceVariableValue
                    (NonNullScalarInputType string) (Aeson.String "asdf")
             in actual `shouldBe` expected
        it "coerces booleans" $
            let expected = Just (Boolean True)
                actual = coerceVariableValue
                    (ScalarInputType boolean) (Aeson.Bool True)
             in actual `shouldBe` expected
        it "coerces zero to an integer" $
            let expected = Just (Int 0)
                actual = coerceVariableValue
                    (ScalarInputType int) (Aeson.Number 0)
             in actual `shouldBe` expected
        it "rejects fractional if an integer is expected" $
            let actual = coerceVariableValue
                    (ScalarInputType int) (Aeson.Number $ scientific 14 (-1))
             in actual `shouldSatisfy` isNothing
        it "coerces float numbers" $
            let expected = Just (Float 1.4)
                actual = coerceVariableValue
                    (ScalarInputType float) (Aeson.Number $ scientific 14 (-1))
             in actual `shouldBe` expected
        it "coerces IDs" $
            let expected = Just (String "1234")
                actual = coerceVariableValue
                    (ScalarInputType id) (Aeson.String "1234")
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
            let actual = coerceVariableValue (ScalarInputType id) Aeson.Null
             in actual `shouldBe` Just Null
        it "preserves list order" $
            let list = Aeson.toJSONList ["asdf" :: Aeson.Value, "qwer"]
                listType = (ListInputType $ ScalarInputType string)
                actual = coerceVariableValue listType list
                expected = Just $ List [String "asdf", String "qwer"]
             in actual `shouldBe` expected
