{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.StarWars.Schema
    ( starWarsSchema
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

starWarsSchema :: Schema (Either SomeException)
starWarsSchema = schema queryType
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
heroObject = Out.ObjectType "Human" Nothing [characterType] $ HashMap.fromList
    [ ("id", idFieldType)
    , ("name", nameFieldType)
    , ("friends", friendsFieldResolver)
    , ("appearsIn", appearsInFieldResolver)
    , ("homePlanet", homePlanetFieldType)
    , ("secretBackstory", secretBackstoryFieldResolver)
    , ("__typename", typenameFieldResolver)
    ]
  where
    homePlanetFieldType
      = ValueResolver (Out.Field Nothing (Out.NamedScalarType string) mempty)
      $ defaultResolver "homePlanet"

droidObject :: Out.ObjectType (Either SomeException)
droidObject = Out.ObjectType "Droid" Nothing [characterType] $ HashMap.fromList
    [ ("id", idFieldType)
    , ("name", nameFieldType)
    , ("friends", friendsFieldResolver)
    , ("appearsIn", appearsInFieldResolver)
    , ("primaryFunction", primaryFunctionFieldType)
    , ("secretBackstory", secretBackstoryFieldResolver)
    , ("__typename", typenameFieldResolver)
    ]
  where
    primaryFunctionFieldType
        = ValueResolver (Out.Field Nothing (Out.NamedScalarType string) mempty)
        $ defaultResolver "primaryFunction"

typenameFieldResolver :: Resolver (Either SomeException)
typenameFieldResolver
    = ValueResolver (Out.Field Nothing (Out.NamedScalarType string) mempty)
    $ defaultResolver "__typename"

idFieldType :: Resolver (Either SomeException)
idFieldType = ValueResolver idField $ defaultResolver "id"

nameFieldType :: Resolver (Either SomeException)
nameFieldType = ValueResolver nameField $ defaultResolver "name"

friendsFieldResolver :: Resolver (Either SomeException)
friendsFieldResolver = ValueResolver friendsField $ defaultResolver "friends"

characterType :: InterfaceType (Either SomeException)
characterType = InterfaceType "Character" Nothing [] $ HashMap.fromList
    [ ("id", idField)
    , ("name", nameField)
    , ("friends", friendsField)
    , ("appearsIn", appearsInField)
    , ("secretBackstory", secretBackstoryField)
    ]

idField :: Field (Either SomeException)
idField = Field Nothing (Out.NonNullScalarType id) mempty

nameField :: Field (Either SomeException)
nameField = Field Nothing (Out.NamedScalarType string) mempty

friendsField :: Field (Either SomeException)
friendsField = Field Nothing friendsFieldType mempty
  where
    friendsFieldType = Out.ListType (Out.NamedInterfaceType characterType)

appearsInField :: Field (Either SomeException)
appearsInField = Field appearsInDescription appearsInFieldType mempty
  where
    appearsInDescription = Just "Which movies they appear in."
    appearsInFieldType = Out.ListType $ Out.NamedEnumType episodeEnum

secretBackstoryField :: Field (Either SomeException)
secretBackstoryField =
    Out.Field Nothing (Out.NamedScalarType string) mempty

appearsInFieldResolver :: Resolver (Either SomeException)
appearsInFieldResolver = ValueResolver appearsInField
    $ defaultResolver "appearsIn"

secretBackstoryFieldResolver :: Resolver (Either SomeException)
secretBackstoryFieldResolver = ValueResolver secretBackstoryField secretBackstory

defaultResolver :: Text -> Resolve (Either SomeException)
defaultResolver f = do
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
