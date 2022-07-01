{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.GraphQL.ExecuteSpec
    ( spec
    ) where

import Control.Exception (Exception(..), SomeException, throwIO)
import Control.Monad.Catch (throwM)
import Data.Conduit
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
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

philosopherSchema :: Schema (Either SomeException)
philosopherSchema =
    schemaWithTypes Nothing queryType Nothing subscriptionRoot extraTypes mempty
  where
    subscriptionRoot = Just subscriptionType
    extraTypes =
        [ Schema.ObjectType bookType
        , Schema.ObjectType bookCollectionType
        ]

queryType :: Out.ObjectType (Either SomeException)
queryType = Out.ObjectType "Query" Nothing []
    $ HashMap.fromList
    [ ("philosopher", ValueResolver philosopherField philosopherResolver)
    , ("genres", ValueResolver genresField genresResolver)
    , ("count", ValueResolver countField countResolver)
    ]
  where
    philosopherField =
        Out.Field Nothing (Out.NamedObjectType philosopherType)
        $ HashMap.singleton "id"
        $ In.Argument Nothing (In.NamedScalarType id) Nothing
    philosopherResolver = pure $ Object mempty
    genresField =
        let fieldType = Out.ListType $ Out.NonNullScalarType string
         in Out.Field Nothing fieldType HashMap.empty
    genresResolver :: Resolve (Either SomeException)
    genresResolver = throwM PhilosopherException
    countField =
        let fieldType = Out.NonNullScalarType int
         in Out.Field Nothing fieldType HashMap.empty
    countResolver = pure ""

musicType :: Out.ObjectType (Either SomeException)
musicType = Out.ObjectType "Music" Nothing []
    $ HashMap.fromList resolvers
  where
    resolvers =
        [ ("instrument", ValueResolver instrumentField instrumentResolver)
        ]
    instrumentResolver = pure $ String "piano"
    instrumentField = Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty

poetryType :: Out.ObjectType (Either SomeException)
poetryType = Out.ObjectType "Poetry" Nothing []
    $ HashMap.fromList resolvers
  where
    resolvers =
        [ ("genre", ValueResolver genreField genreResolver)
        ]
    genreResolver = pure $ String "Futurism"
    genreField = Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty

interestType :: Out.UnionType (Either SomeException)
interestType = Out.UnionType "Interest" Nothing [musicType, poetryType]

philosopherType :: Out.ObjectType (Either SomeException)
philosopherType = Out.ObjectType "Philosopher" Nothing []
    $ HashMap.fromList resolvers
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
        Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty
    firstNameResolver = pure $ String "Friedrich"
    lastNameField
        = Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty
    lastNameResolver = pure $ String "Nietzsche"
    schoolField
       = Out.Field Nothing (Out.NonNullEnumType schoolType) HashMap.empty
    schoolResolver = pure $ Enum "EXISTENTIALISM"
    interestField
        = Out.Field Nothing (Out.NonNullUnionType interestType) HashMap.empty
    interestResolver = pure
        $ Object
        $ HashMap.fromList [("instrument", "piano")]
    majorWorkField
        = Out.Field Nothing (Out.NonNullInterfaceType workType) HashMap.empty
    majorWorkResolver = pure
        $ Object
        $ HashMap.fromList
            [ ("title", "Also sprach Zarathustra: Ein Buch für Alle und Keinen")
            ]
    centuryField =
        Out.Field Nothing (Out.NonNullScalarType int) HashMap.empty
    centuryResolver = pure $ Float 18.5
    firstLanguageField
        = Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty
    firstLanguageResolver = pure Null

workType :: Out.InterfaceType (Either SomeException)
workType = Out.InterfaceType "Work" Nothing []
    $ HashMap.fromList fields
  where
    fields = [("title", titleField)]
    titleField = Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty

bookType :: Out.ObjectType (Either SomeException)
bookType = Out.ObjectType "Book" Nothing [workType]
    $ HashMap.fromList resolvers
  where
    resolvers =
        [ ("title", ValueResolver titleField titleResolver)
        ]
    titleField = Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty
    titleResolver = pure "Also sprach Zarathustra: Ein Buch für Alle und Keinen"

bookCollectionType :: Out.ObjectType (Either SomeException)
bookCollectionType = Out.ObjectType "Book" Nothing [workType]
    $ HashMap.fromList resolvers
  where
    resolvers =
        [ ("title", ValueResolver titleField titleResolver)
        ]
    titleField = Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty
    titleResolver = pure "The Three Critiques"

subscriptionType :: Out.ObjectType (Either SomeException)
subscriptionType = Out.ObjectType "Subscription" Nothing []
    $ HashMap.singleton "newQuote"
    $ EventStreamResolver quoteField (pure $ Object mempty)
    $ pure $ yield $ Object mempty
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

schoolType :: Type.EnumType
schoolType = EnumType "School" Nothing $ HashMap.fromList
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
            case execute philosopherSchema Nothing (mempty :: HashMap Name Type.Value) parsedDocument of
                Right (Right result) -> shouldBe result expected
                Right (Left _) -> expectationFailure
                    "the query is expected to resolve to a value, but it resolved to an event stream"
                Left executionError -> throwIO executionError
        (Left errorBundle) -> expectationFailure $ errorBundlePretty errorBundle

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
                        $ HashMap.singleton "philosopher"
                        $ Object
                        $ HashMap.singleton "firstName"
                        $ String "Friedrich"
                    expected = Response data'' mempty
                    sourceQuery = "{ philosopher { firstName surname } }"
                in sourceQuery `shouldResolveTo` expected
            it "merges selections" $
                let data'' = Object
                        $ HashMap.singleton "philosopher"
                        $ Object
                        $ HashMap.fromList
                            [ ("firstName", String "Friedrich")
                            , ("lastName", String "Nietzsche")
                            ]
                    expected = Response data'' mempty
                    sourceQuery = "{ philosopher { firstName } philosopher { lastName } }"
                in sourceQuery `shouldResolveTo` expected

            it "errors on invalid output enum values" $
                let data'' = Object $ HashMap.singleton "philosopher" Null
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
                let data'' = Object $ HashMap.singleton "philosopher" Null
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
                let data'' = Object $ HashMap.singleton "philosopher" Null
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
                let data'' = Object $ HashMap.singleton "philosopher" Null
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
                let data'' = Object $ HashMap.singleton "philosopher" Null
                    executionErrors = pure $ Error
                        { message = "Unable to coerce result to !Int."
                        , locations = [Location 1 26]
                        , path = [Segment "philosopher", Segment "century"]
                        }
                    expected = Response data'' executionErrors
                    sourceQuery = "{ philosopher(id: \"1\") { century } }"
                in sourceQuery `shouldResolveTo` expected

            it "gives location information for failed result coercion" $
                let data'' = Object $ HashMap.singleton "genres" Null
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
                let data'' = Object $ HashMap.singleton "philosopher" Null
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
                    startsWith :: Text.Text -> Text.Text -> Bool
                    startsWith xs ys = Text.take (Text.length ys) xs == ys

                it "throws operation name is required error" $
                    let expectedErrorMessage :: Text.Text
                        expectedErrorMessage = "Operation name is required"
                        execute' :: Document -> Either SomeException EitherStreamOrValue
                        execute' = execute philosopherSchema Nothing (mempty :: HashMap Name Type.Value)
                        Right (Right (Response _ executionErrors)) = either (pure . parseError) execute'
                            $ parse document "" twoQueries 
                        Error msg _ _ = Seq.index executionErrors 0
                     in msg `startsWith` expectedErrorMessage `shouldBe` True

                it "throws operation not found error" $
                    let expectedErrorMessage :: Text.Text
                        expectedErrorMessage = "Operation \"C\" is not found"
                        execute' :: Document -> Either SomeException EitherStreamOrValue
                        execute' = execute philosopherSchema (Just "C") (mempty :: HashMap Name Type.Value)
                        Right (Right (Response _ executionErrors)) = either (pure . parseError) execute'
                            $ parse document "" twoQueries
                        Error msg _ _ = Seq.index executionErrors 0
                     in msg `startsWith` expectedErrorMessage `shouldBe` True

                it "throws variable coercion error" $
                    let data'' = Null
                        executionErrors = pure $ Error
                            { message = "Failed to coerce the variable $id: String."
                            , locations =[Location 1 7]
                            , path = []
                            }
                        expected = Response data'' executionErrors
                        executeWithVars :: Document -> Either SomeException EitherStreamOrValue
                        executeWithVars = execute philosopherSchema Nothing (HashMap.singleton "id" (Type.Int 1))
                        Right (Right actual) = either (pure . parseError) executeWithVars
                            $ parse document "" "query($id: String) { philosopher(id: \"1\") { firstLanguage } }"
                    in actual `shouldBe` expected

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
                    executeHero = execute heroSchema Nothing (HashMap.empty :: HashMap Name Type.Value)

                it "at the beggining of the list" $
                    let Right (Right actual) = either (pure . parseError) executeHero
                            $ parse document "" "{ hero(id: \"1\") { friends { name } } }"
                        Response _ errors' = actual
                        Error _ _ path' = fromJust $ Seq.lookup 0 errors'
                        expected = [Segment "hero", Segment "friends", Index 0, Segment "name"]
                    in path' `shouldBe` expected

        context "Subscription" $
            it "subscribes" $
                let data'' = Object
                        $ HashMap.singleton "newQuote"
                        $ Object
                        $ HashMap.singleton "quote"
                        $ String "Naturam expelles furca, tamen usque recurret."
                    expected = Response data'' mempty
                    Left stream
                        = fromRight (error "Execution error")
                        $ execute philosopherSchema Nothing (mempty :: HashMap Name Type.Value)
                        $ fromRight (error "Parse error")
                        $ parse document "" "subscription { newQuote { quote } }"
                    Right (Just actual) = runConduit $ stream .| await
                in actual `shouldBe` expected
