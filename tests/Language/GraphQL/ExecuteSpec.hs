{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.GraphQL.ExecuteSpec
    ( spec
    ) where

import Control.Exception (Exception(..), SomeException)
import Control.Monad.Catch (throwM)
import Data.Conduit
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.Key as Key
import Data.Typeable (cast)
import Language.GraphQL.AST (Document, Location(..), Name)
import Language.GraphQL.AST.Parser (document)
import Language.GraphQL.Error
import Language.GraphQL.Execute (execute)
import Language.GraphQL.TH
import qualified Language.GraphQL.Type.Schema as Schema
import qualified Language.GraphQL.Type as Type
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Prelude hiding (id)
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Text.Megaparsec (parse, errorBundlePretty)
import Schemas.HeroSchema (heroSchema)
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec.Expectations
    ( Expectation
    , expectationFailure
    )
import Data.Either (fromRight)

data PhilosopherException = PhilosopherException
    deriving Show

instance Exception PhilosopherException where
    toException = toException. ResolverException
    fromException e = do
        ResolverException resolverException <- fromException e
        cast resolverException

philosopherSchema :: Schema IO
philosopherSchema =
    schemaWithTypes Nothing queryType Nothing subscriptionRoot extraTypes mempty
  where
    subscriptionRoot = Just subscriptionType
    extraTypes =
        [ Schema.ObjectType bookType
        , Schema.ObjectType bookCollectionType
        ]

queryType :: Out.ObjectType IO
queryType = Out.ObjectType "Query" Nothing []
    $ KeyMap.fromList
    [ ("philosopher", ValueResolver philosopherField philosopherResolver)
    , ("genres", ValueResolver genresField genresResolver)
    , ("count", ValueResolver countField countResolver)
    ]
  where
    philosopherField =
        Out.Field Nothing (Out.NamedObjectType philosopherType)
        $ KeyMap.singleton "id"
        $ In.Argument Nothing (In.NamedScalarType id) Nothing
    philosopherResolver = pure $ Object mempty
    genresField =
        let fieldType = Out.ListType $ Out.NonNullScalarType string
         in Out.Field Nothing fieldType KeyMap.empty
    genresResolver :: Resolve IO
    genresResolver = throwM PhilosopherException
    countField =
        let fieldType = Out.NonNullScalarType int
         in Out.Field Nothing fieldType KeyMap.empty
    countResolver = pure ""

musicType :: Out.ObjectType IO
musicType = Out.ObjectType "Music" Nothing []
    $ KeyMap.fromList resolvers
  where
    resolvers =
        [ ("instrument", ValueResolver instrumentField instrumentResolver)
        ]
    instrumentResolver = pure $ String "piano"
    instrumentField = Out.Field Nothing (Out.NonNullScalarType string) KeyMap.empty

poetryType :: Out.ObjectType IO
poetryType = Out.ObjectType "Poetry" Nothing []
    $ KeyMap.fromList resolvers
  where
    resolvers =
        [ ("genre", ValueResolver genreField genreResolver)
        ]
    genreResolver = pure $ String "Futurism"
    genreField = Out.Field Nothing (Out.NonNullScalarType string) KeyMap.empty

interestType :: Out.UnionType IO
interestType = Out.UnionType "Interest" Nothing [musicType, poetryType]

philosopherType :: Out.ObjectType IO
philosopherType = Out.ObjectType "Philosopher" Nothing []
    $ KeyMap.fromList resolvers
  where
    resolvers =
        [ ("firstName", ValueResolver firstNameField firstNameResolver)
        , ("lastName", ValueResolver lastNameField lastNameResolver)
        , ("school", ValueResolver schoolField schoolResolver)
        , ("interest", ValueResolver interestField interestResolver)
        , ("majorWork", ValueResolver majorWorkField majorWorkResolver)
        , ("century", ValueResolver centuryField centuryResolver)
        , ("firstLanguage", ValueResolver firstLanguageField firstLanguageResolver)
        ]
    firstNameField =
        Out.Field Nothing (Out.NonNullScalarType string) KeyMap.empty
    firstNameResolver = pure $ String "Friedrich"
    lastNameField
        = Out.Field Nothing (Out.NonNullScalarType string) KeyMap.empty
    lastNameResolver = pure $ String "Nietzsche"
    schoolField
       = Out.Field Nothing (Out.NonNullEnumType schoolType) KeyMap.empty
    schoolResolver = pure $ Enum "EXISTENTIALISM"
    interestField
        = Out.Field Nothing (Out.NonNullUnionType interestType) KeyMap.empty
    interestResolver = pure
        $ Object
        $ KeyMap.fromList [("instrument", "piano")]
    majorWorkField
        = Out.Field Nothing (Out.NonNullInterfaceType workType) KeyMap.empty
    majorWorkResolver = pure
        $ Object
        $ KeyMap.fromList
            [ ("title", "Also sprach Zarathustra: Ein Buch für Alle und Keinen")
            ]
    centuryField =
        Out.Field Nothing (Out.NonNullScalarType int) KeyMap.empty
    centuryResolver = pure $ Float 18.5
    firstLanguageField
        = Out.Field Nothing (Out.NonNullScalarType string) KeyMap.empty
    firstLanguageResolver = pure Null

workType :: Out.InterfaceType IO
workType = Out.InterfaceType "Work" Nothing []
    $ KeyMap.fromList fields
  where
    fields = [("title", titleField)]
    titleField = Out.Field Nothing (Out.NonNullScalarType string) KeyMap.empty

bookType :: Out.ObjectType IO
bookType = Out.ObjectType "Book" Nothing [workType]
    $ KeyMap.fromList resolvers
  where
    resolvers =
        [ ("title", ValueResolver titleField titleResolver)
        ]
    titleField = Out.Field Nothing (Out.NonNullScalarType string) KeyMap.empty
    titleResolver = pure "Also sprach Zarathustra: Ein Buch für Alle und Keinen"

bookCollectionType :: Out.ObjectType IO
bookCollectionType = Out.ObjectType "Book" Nothing [workType]
    $ KeyMap.fromList resolvers
  where
    resolvers =
        [ ("title", ValueResolver titleField titleResolver)
        ]
    titleField = Out.Field Nothing (Out.NonNullScalarType string) KeyMap.empty
    titleResolver = pure "The Three Critiques"

subscriptionType :: Out.ObjectType IO
subscriptionType = Out.ObjectType "Subscription" Nothing []
    $ KeyMap.singleton "newQuote"
    $ EventStreamResolver quoteField (pure $ Object mempty)
    $ pure $ yield $ Object mempty
  where
    quoteField =
        Out.Field Nothing (Out.NonNullObjectType quoteType) KeyMap.empty

quoteType :: Out.ObjectType IO
quoteType = Out.ObjectType "Quote" Nothing []
    $ KeyMap.singleton "quote"
    $ ValueResolver quoteField
    $ pure "Naturam expelles furca, tamen usque recurret."
  where
    quoteField =
        Out.Field Nothing (Out.NonNullScalarType string) KeyMap.empty

schoolType :: Type.EnumType
schoolType = EnumType "School" Nothing $ KeyMap.fromList
    [ ("NOMINALISM", EnumValue Nothing)
    , ("REALISM", EnumValue Nothing)
    , ("IDEALISM", EnumValue Nothing)
    ]

type EitherStreamOrValue = Either
    (ResponseEventStream (Either SomeException) Type.Value)
    (Response Type.Value)

-- Asserts that a query resolves to a value.
shouldResolveTo :: Text.Text -> Response Type.Value -> Expectation
shouldResolveTo querySource expected =
    case parse document "" querySource of
        (Right parsedDocument) ->
            execute philosopherSchema Nothing (mempty :: KeyMap Type.Value) parsedDocument >>= go
        (Left errorBundle) -> expectationFailure $ errorBundlePretty errorBundle
  where
    go = \case
        Right result -> shouldBe result expected
        Left _ -> expectationFailure
            "the query is expected to resolve to a value, but it resolved to an event stream"

-- Asserts that the executor produces an error that starts with a string.
shouldContainError :: Either (ResponseEventStream IO Type.Value) (Response Type.Value)
    -> Text
    -> Expectation
shouldContainError streamOrValue expected =
    case streamOrValue of
        Right response -> respond response
        Left _ -> expectationFailure
            "the query is expected to resolve to a value, but it resolved to an event stream"
  where
    startsWith :: Text.Text -> Text.Text -> Bool
    startsWith xs ys = Text.take (Text.length ys) xs == ys
    respond :: Response Type.Value -> Expectation
    respond Response{ errors }
        | any ((`startsWith` expected) . message) errors = pure ()
        | otherwise = expectationFailure
            "the query is expected to execute with errors, but the response doesn't contain any errors"

parseAndExecute :: Schema IO
    -> Maybe Text
    -> KeyMap Type.Value
    -> Text
    -> IO (Either (ResponseEventStream IO Type.Value) (Response Type.Value))
parseAndExecute schema' operation variables
    = either (pure . parseError) (execute schema' operation variables)
    . parse document ""

spec :: Spec
spec =
    describe "execute" $ do
        it "rejects recursive fragments" $
            let sourceQuery = [gql|
              {
                ...cyclicFragment
              }

              fragment cyclicFragment on Query {
                ...cyclicFragment
              }
            |]
                expected = Response (Object mempty) mempty
             in sourceQuery `shouldResolveTo` expected

        context "Query" $ do
            it "skips unknown fields" $
                let data'' = Object
                        $ KeyMap.singleton "philosopher"
                        $ Object
                        $ KeyMap.singleton "firstName"
                        $ String "Friedrich"
                    expected = Response data'' mempty
                    sourceQuery = "{ philosopher { firstName surname } }"
                in sourceQuery `shouldResolveTo` expected
            it "merges selections" $
                let data'' = Object
                        $ KeyMap.singleton "philosopher"
                        $ Object
                        $ KeyMap.fromList
                            [ ("firstName", String "Friedrich")
                            , ("lastName", String "Nietzsche")
                            ]
                    expected = Response data'' mempty
                    sourceQuery = "{ philosopher { firstName } philosopher { lastName } }"
                in sourceQuery `shouldResolveTo` expected

            it "errors on invalid output enum values" $
                let data'' = Object $ KeyMap.singleton "philosopher" Null
                    executionErrors = pure $ Error
                        { message =
                            "Value completion error. Expected type !School, found: EXISTENTIALISM."
                        , locations = [Location 1 17]
                        , path = [Segment "philosopher", Segment "school"]
                        }
                    expected = Response data'' executionErrors
                    sourceQuery = "{ philosopher { school } }"
                 in sourceQuery `shouldResolveTo` expected

            it "gives location information for non-null unions" $
                let data'' = Object $ KeyMap.singleton "philosopher" Null
                    executionErrors = pure $ Error
                        { message =
                            "Value completion error. Expected type !Interest, found: { instrument: \"piano\" }."
                        , locations = [Location 1 17]
                        , path = [Segment "philosopher", Segment "interest"]
                        }
                    expected = Response data'' executionErrors
                    sourceQuery = "{ philosopher { interest } }"
                 in sourceQuery `shouldResolveTo` expected

            it "gives location information for invalid interfaces" $
                let data'' = Object $ KeyMap.singleton "philosopher" Null
                    executionErrors = pure $ Error
                        { message
                            = "Value completion error. Expected type !Work, found:\
                            \ { title: \"Also sprach Zarathustra: Ein Buch f\252r Alle und Keinen\" }."
                        , locations = [Location 1 17]
                        , path = [Segment "philosopher", Segment "majorWork"]
                        }
                    expected = Response data'' executionErrors
                    sourceQuery = "{ philosopher { majorWork { title } } }"
                 in sourceQuery `shouldResolveTo` expected

            it "gives location information for invalid scalar arguments" $
                let data'' = Object $ KeyMap.singleton "philosopher" Null
                    executionErrors = pure $ Error
                        { message =
                            "Argument \"id\" has invalid type. Expected type ID, found: True."
                        , locations = [Location 1 15]
                        , path = [Segment "philosopher"]
                        }
                    expected = Response data'' executionErrors
                    sourceQuery = "{ philosopher(id: true) { lastName } }"
                 in sourceQuery `shouldResolveTo` expected

            it "gives location information for failed result coercion" $
                let data'' = Object $ KeyMap.singleton "philosopher" Null
                    executionErrors = pure $ Error
                        { message = "Unable to coerce result to !Int."
                        , locations = [Location 1 26]
                        , path = [Segment "philosopher", Segment "century"]
                        }
                    expected = Response data'' executionErrors
                    sourceQuery = "{ philosopher(id: \"1\") { century } }"
                in sourceQuery `shouldResolveTo` expected

            it "gives location information for failed result coercion" $
                let data'' = Object $ KeyMap.singleton "genres" Null
                    executionErrors = pure $ Error
                        { message = "PhilosopherException"
                        , locations = [Location 1 3]
                        , path = [Segment "genres"]
                        }
                    expected = Response data'' executionErrors
                    sourceQuery = "{ genres }"
                in sourceQuery `shouldResolveTo` expected

            it "sets data to null if a root field isn't nullable" $
                let executionErrors = pure $ Error
                        { message = "Unable to coerce result to !Int."
                        , locations = [Location 1 3]
                        , path = [Segment "count"]
                        }
                    expected = Response Null executionErrors
                    sourceQuery = "{ count }"
                in sourceQuery `shouldResolveTo` expected

            it "detects nullability errors" $
                let data'' = Object $ KeyMap.singleton "philosopher" Null
                    executionErrors = pure $ Error
                        { message = "Value completion error. Expected type !String, found: null."
                        , locations = [Location 1 26]
                        , path = [Segment "philosopher", Segment "firstLanguage"]
                        }
                    expected = Response data'' executionErrors
                    sourceQuery = "{ philosopher(id: \"1\") { firstLanguage } }"
                in sourceQuery `shouldResolveTo` expected

            context "queryError" $ do
                let namedQuery name = "query " <> name <> " { philosopher(id: \"1\") { interest } }"
                    twoQueries = namedQuery "A" <> " " <> namedQuery "B"

                it "throws operation name is required error" $ do
                    let expectedErrorMessage = "Operation name is required"
                    actual <- parseAndExecute philosopherSchema Nothing mempty twoQueries
                    actual `shouldContainError` expectedErrorMessage

                it "throws operation not found error" $ do
                    let expectedErrorMessage = "Operation \"C\" is not found"
                    actual <- parseAndExecute philosopherSchema (Just "C") mempty twoQueries
                    actual `shouldContainError` expectedErrorMessage

                it "throws variable coercion error" $ do
                    let data'' = Null
                        executionErrors = pure $ Error
                            { message = "Failed to coerce the variable $id: String."
                            , locations =[Location 1 7]
                            , path = []
                            }
                        expected = Response data'' executionErrors
                        executeWithVars = execute philosopherSchema Nothing (KeyMap.singleton "id" (Type.Int 1))
                    Right actual <- either (pure . parseError) executeWithVars
                        $ parse document "" "query($id: String) { philosopher(id: \"1\") { firstLanguage } }"
                    actual `shouldBe` expected

                it "throws variable unkown input type error" $
                    let data'' = Null
                        executionErrors = pure $ Error
                            { message = "Variable $id has unknown type Cat."
                            , locations =[Location 1 7]
                            , path = []
                            }
                        expected = Response data'' executionErrors
                        sourceQuery = "query($id: Cat) { philosopher(id: \"1\") { firstLanguage } }"
                     in sourceQuery `shouldResolveTo` expected

            context "Error path" $ do
                let executeHero :: Document -> Either SomeException EitherStreamOrValue
                    executeHero = execute heroSchema Nothing (KeyMap.empty :: KeyMap Type.Value)

                it "at the beggining of the list" $
                    let Right (Right actual) = either (pure . parseError) executeHero
                            $ parse document "" "{ hero(id: \"1\") { friends { name } } }"
                        Response _ errors' = actual
                        Error _ _ path' = fromJust $ Seq.lookup 0 errors'
                        expected = [Segment "hero", Segment "friends", Index 0, Segment "name"]
                    in path' `shouldBe` expected

        context "Subscription" $
            it "subscribes" $ do
                let data'' = Object
                        $ KeyMap.singleton "newQuote"
                        $ Object
                        $ KeyMap.singleton "quote"
                        $ String "Naturam expelles furca, tamen usque recurret."
                    expected = Response data'' mempty
                Left stream <- execute philosopherSchema Nothing (mempty :: KeyMap Type.Value)
                    $ fromRight (error "Parse error")
                    $ parse document "" "subscription { newQuote { quote } }"
                Just actual <- runConduit $ stream .| await
                actual `shouldBe` expected
