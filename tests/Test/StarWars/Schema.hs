{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.StarWars.Schema
    ( schema
    ) where

import Control.Monad.Catch (MonadThrow(..), SomeException)
import Control.Monad.Trans.Reader (asks)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Test.StarWars.Data
import Prelude hiding (id)

-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: Schema (Either SomeException)
schema = Schema
    { query = queryType
    , mutation = Nothing
    , subscription = Nothing
    }
  where
    queryType = Out.ObjectType "Query" Nothing [] $ HashMap.fromList
        [ ("hero", heroFieldResolver)
        , ("human", humanFieldResolver)
        , ("droid", droidFieldResolver)
        ]
    heroField = Out.Field Nothing (Out.NamedObjectType heroObject)
        $ HashMap.singleton "episode"
        $ In.Argument Nothing (In.NamedEnumType episodeEnum) Nothing
    heroFieldResolver = ValueResolver heroField hero
    humanField = Out.Field Nothing (Out.NamedObjectType heroObject)
        $ HashMap.singleton "id"
        $ In.Argument Nothing (In.NonNullScalarType string) Nothing
    humanFieldResolver = ValueResolver humanField human
    droidField = Out.Field Nothing (Out.NamedObjectType droidObject) mempty
    droidFieldResolver = ValueResolver droidField droid

heroObject :: Out.ObjectType (Either SomeException)
heroObject = Out.ObjectType "Human" Nothing [] $ HashMap.fromList
    [ ("id", idFieldType)
    , ("name", nameFieldType)
    , ("friends", friendsFieldType)
    , ("appearsIn", appearsInField)
    , ("homePlanet", homePlanetFieldType)
    , ("secretBackstory", secretBackstoryFieldType)
    , ("__typename", typenameFieldType)
    ]
  where
    homePlanetFieldType
      = ValueResolver (Out.Field Nothing (Out.NamedScalarType string) mempty)
      $ idField "homePlanet"

droidObject :: Out.ObjectType (Either SomeException)
droidObject = Out.ObjectType "Droid" Nothing [] $ HashMap.fromList
    [ ("id", idFieldType)
    , ("name", nameFieldType)
    , ("friends", friendsFieldType)
    , ("appearsIn", appearsInField)
    , ("primaryFunction", primaryFunctionFieldType)
    , ("secretBackstory", secretBackstoryFieldType)
    , ("__typename", typenameFieldType)
    ]
  where
    primaryFunctionFieldType
        = ValueResolver (Out.Field Nothing (Out.NamedScalarType string) mempty)
        $ idField "primaryFunction"

typenameFieldType :: Resolver (Either SomeException)
typenameFieldType
    = ValueResolver (Out.Field Nothing (Out.NamedScalarType string) mempty)
    $ idField "__typename"

idFieldType :: Resolver (Either SomeException)
idFieldType
    = ValueResolver (Out.Field Nothing (Out.NamedScalarType id) mempty)
    $ idField "id"

nameFieldType :: Resolver (Either SomeException)
nameFieldType
    = ValueResolver (Out.Field Nothing (Out.NamedScalarType string) mempty)
    $ idField "name"

friendsFieldType :: Resolver (Either SomeException)
friendsFieldType
    = ValueResolver (Out.Field Nothing fieldType mempty)
    $ idField "friends"
  where
    fieldType = Out.ListType $ Out.NamedObjectType droidObject

appearsInField :: Resolver (Either SomeException)
appearsInField
    = ValueResolver (Out.Field (Just description) fieldType mempty)
    $ idField "appearsIn"
  where
    fieldType = Out.ListType $ Out.NamedEnumType episodeEnum
    description = "Which movies they appear in."

secretBackstoryFieldType :: Resolver (Either SomeException)
secretBackstoryFieldType = ValueResolver field secretBackstory
  where
    field = Out.Field Nothing (Out.NamedScalarType string) mempty

idField :: Text -> Resolve (Either SomeException)
idField f = do
    v <- asks values
    let (Object v') = v
    pure $ v' HashMap.! f

episodeEnum :: EnumType
episodeEnum = EnumType "Episode" (Just description)
    $ HashMap.fromList [newHope, empire, jedi]
  where
    description = "One of the films in the Star Wars Trilogy"
    newHope = ("NEW_HOPE", EnumValue $ Just "Released in 1977.")
    empire = ("EMPIRE", EnumValue $ Just "Released in 1980.")
    jedi = ("JEDI", EnumValue $ Just "Released in 1983.")

hero :: Resolve (Either SomeException)
hero = do
  episode <- argument "episode"
  pure $ character $ case episode of
      Enum "NEW_HOPE" -> getHero 4
      Enum "EMPIRE" -> getHero 5
      Enum "JEDI" -> getHero 6
      _ -> artoo

human :: Resolve (Either SomeException)
human = do
    id' <- argument "id"
    case id' of
        String i -> pure $ maybe Null character $ getHuman i >>= Just
        _ -> throwM InvalidArguments

droid :: Resolve (Either SomeException)
droid = do
    id' <- argument "id"
    case id' of
        String i -> pure $ maybe Null character $ getDroid i >>= Just
        _ -> throwM InvalidArguments

character :: Character -> Value
character char = Object $ HashMap.fromList
    [ ("id", String $ id_ char)
    , ("name", String $ name_ char)
    , ("friends", List $ character <$> getFriends char)
    , ("appearsIn", List $ Enum <$> catMaybes (getEpisode <$> appearsIn char))
    , ("homePlanet", String $ either mempty homePlanet char)
    , ("__typename", String $ typeName char)
    ]
