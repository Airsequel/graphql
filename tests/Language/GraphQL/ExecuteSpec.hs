{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.GraphQL.ExecuteSpec
    ( spec
    ) where

import Control.Exception (SomeException)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (emptyObject)
import Data.Conduit
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL.AST (Document, Name)
import Language.GraphQL.AST.Parser (document)
import Language.GraphQL.Error
import Language.GraphQL.Execute
import Language.GraphQL.Type as Type
import Language.GraphQL.Type.Out as Out
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

philosopherSchema :: Schema (Either SomeException)
philosopherSchema = schema queryType Nothing (Just subscriptionType) mempty

queryType :: Out.ObjectType (Either SomeException)
queryType = Out.ObjectType "Query" Nothing []
    $ HashMap.singleton "philosopher"
    $ ValueResolver philosopherField
    $ pure $ Type.Object mempty
  where
    philosopherField =
        Out.Field Nothing (Out.NonNullObjectType philosopherType) HashMap.empty

philosopherType :: Out.ObjectType (Either SomeException)
philosopherType = Out.ObjectType "Philosopher" Nothing []
    $ HashMap.fromList resolvers
  where
    resolvers =
        [ ("firstName", ValueResolver firstNameField firstNameResolver)
        , ("lastName", ValueResolver lastNameField lastNameResolver)
        , ("school", ValueResolver schoolField schoolResolver)
        ]
    firstNameField =
        Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty
    firstNameResolver = pure $ Type.String "Friedrich"
    lastNameField
        = Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty
    lastNameResolver = pure $ Type.String "Nietzsche"
    schoolField
       = Out.Field Nothing (Out.NonNullEnumType schoolType) HashMap.empty
    schoolResolver = pure $ Type.Enum "EXISTENTIALISM"

subscriptionType :: Out.ObjectType (Either SomeException)
subscriptionType = Out.ObjectType "Subscription" Nothing []
    $ HashMap.singleton "newQuote"
    $ EventStreamResolver quoteField (pure $ Type.Object mempty)
    $ pure $ yield $ Type.Object mempty
  where
    quoteField =
        Out.Field Nothing (Out.NonNullObjectType quoteType) HashMap.empty

quoteType :: Out.ObjectType (Either SomeException)
quoteType = Out.ObjectType "Quote" Nothing []
    $ HashMap.singleton "quote"
    $ ValueResolver quoteField
    $ pure "Naturam expelles furca, tamen usque recurret."
  where
    quoteField =
        Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty

schoolType :: EnumType
schoolType = EnumType "School" Nothing $ HashMap.fromList
    [ ("NOMINALISM", EnumValue Nothing)
    , ("REALISM", EnumValue Nothing)
    , ("IDEALISM", EnumValue Nothing)
    ]

type EitherStreamOrValue = Either
    (ResponseEventStream (Either SomeException) Aeson.Value)
    (Response Aeson.Value)

execute' :: Document -> Either SomeException EitherStreamOrValue
execute' =
    execute philosopherSchema Nothing (mempty :: HashMap Name Aeson.Value)

spec :: Spec
spec =
    describe "execute" $ do
        it "rejects recursive fragments" $
            let sourceQuery = [r|
              {
                ...cyclicFragment
              }

              fragment cyclicFragment on Query {
                ...cyclicFragment
              }
            |]
                expected = Response emptyObject mempty
                Right (Right actual) = either (pure . parseError) execute'
                    $ parse document "" sourceQuery
             in actual `shouldBe` expected

        context "Query" $ do
            it "skips unknown fields" $
                let data'' = Aeson.object
                        [ "philosopher" .= Aeson.object
                            [ "firstName" .= ("Friedrich" :: String)
                            ]
                        ]
                    expected = Response data'' mempty
                    Right (Right actual) = either (pure . parseError) execute'
                        $ parse document "" "{ philosopher { firstName surname } }"
                in actual `shouldBe` expected
            it "merges selections" $
                let data'' = Aeson.object
                        [ "philosopher" .= Aeson.object
                            [ "firstName" .= ("Friedrich" :: String)
                            , "lastName" .= ("Nietzsche" :: String)
                            ]
                        ]
                    expected = Response data'' mempty
                    Right (Right actual) = either (pure . parseError) execute'
                        $ parse document "" "{ philosopher { firstName } philosopher { lastName } }"
                in actual `shouldBe` expected

            it "errors on invalid output enum values" $
                let data'' = Aeson.object
                        [ "philosopher" .= Aeson.object
                            [ "school" .= Aeson.Null
                            ]
                        ]
                    executionErrors = pure $ Error
                        { message = "Enum value completion failed."
                        , locations = []
                        , path = []
                        }
                    expected = Response data'' executionErrors
                    Right (Right actual) = either (pure . parseError) execute'
                        $ parse document "" "{ philosopher { school } }"
                in actual `shouldBe` expected

        context "Subscription" $
            it "subscribes" $
                let data'' = Aeson.object
                        [ "newQuote" .= Aeson.object
                            [ "quote" .= ("Naturam expelles furca, tamen usque recurret." :: String)
                            ]
                        ]
                    expected = Response data'' mempty
                    Right (Left stream) = either (pure . parseError) execute'
                        $ parse document "" "subscription { newQuote { quote } }"
                    Right (Just actual) = runConduit $ stream .| await
                in actual `shouldBe` expected
