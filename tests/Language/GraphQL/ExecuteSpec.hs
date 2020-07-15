{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.ExecuteSpec
    ( spec
    ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Conduit
import Data.Either (fromRight)
import Data.Functor.Identity (Identity(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL.AST (Name)
import Language.GraphQL.AST.Parser (document)
import Language.GraphQL.Error
import Language.GraphQL.Execute
import Language.GraphQL.Type as Type
import Language.GraphQL.Type.Out as Out
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Text.Megaparsec (parse)

schema :: Schema Identity
schema = Schema
    { query = queryType
    , mutation = Nothing
    , subscription = Just subscriptionType
    }

queryType :: Out.ObjectType Identity
queryType = Out.ObjectType "Query" Nothing []
    $ HashMap.singleton "philosopher" 
    $ ValueResolver philosopherField
    $ pure $ Type.Object mempty
  where
    philosopherField =
        Out.Field Nothing (Out.NonNullObjectType philosopherType) HashMap.empty

philosopherType :: Out.ObjectType Identity
philosopherType = Out.ObjectType "Philosopher" Nothing []
    $ HashMap.fromList resolvers
  where
    resolvers =
        [ ("firstName", ValueResolver firstNameField firstNameResolver)
        , ("lastName", ValueResolver lastNameField lastNameResolver)
        ]
    firstNameField =
        Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty
    firstNameResolver = pure $ Type.String "Friedrich"
    lastNameField
        = Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty
    lastNameResolver = pure $ Type.String "Nietzsche"

subscriptionType :: Out.ObjectType Identity
subscriptionType = Out.ObjectType "Subscription" Nothing []
    $ HashMap.singleton "newQuote"
    $ EventStreamResolver quoteField (pure $ Type.Object mempty)
    $ pure $ yield $ Type.Object mempty
  where
    quoteField =
        Out.Field Nothing (Out.NonNullObjectType quoteType) HashMap.empty

quoteType :: Out.ObjectType Identity
quoteType = Out.ObjectType "Quote" Nothing []
    $ HashMap.singleton "quote"
    $ ValueResolver quoteField
    $ pure "Naturam expelles furca, tamen usque recurret."
  where
    quoteField =
        Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty

spec :: Spec
spec =
    describe "execute" $ do
        context "Query" $ do
            it "skips unknown fields" $
                let data'' = Aeson.object
                        [ "philosopher" .= Aeson.object
                            [ "firstName" .= ("Friedrich" :: String)
                            ]
                        ]
                    expected = Response data'' mempty
                    execute' = execute schema Nothing (mempty :: HashMap Name Aeson.Value)
                    actual = fromRight (singleError "")
                        $ runIdentity
                        $ either (pure . parseError) execute'
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
                    execute' = execute schema Nothing (mempty :: HashMap Name Aeson.Value)
                    actual = fromRight (singleError "")
                        $ runIdentity
                        $ either (pure . parseError) execute'
                        $ parse document "" "{ philosopher { firstName } philosopher { lastName } }"
                in actual `shouldBe` expected
        context "Subscription" $
            it "subscribes" $
                let data'' = Aeson.object
                        [ "newQuote" .= Aeson.object
                            [ "quote" .= ("Naturam expelles furca, tamen usque recurret." :: String)
                            ]
                        ]
                    expected = Response data'' mempty
                    execute' = execute schema Nothing (mempty :: HashMap Name Aeson.Value)
                    Left stream = runIdentity
                        $ either (pure . parseError) execute'
                        $ parse document "" "subscription { newQuote { quote } }"
                    Just actual = runConduitPure $ stream .| await
                in actual `shouldBe` expected
