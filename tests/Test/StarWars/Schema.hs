{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.StarWars.Schema
    ( schema
    ) where

import Control.Monad.Trans.Reader (asks)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Language.GraphQL.Type
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Test.StarWars.Data
import Prelude hiding (id)

-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: Schema Identity
schema = Schema { query = queryType, mutation = Nothing }
  where
    queryType = Out.ObjectType "Query" Nothing [] $ HashMap.fromList
        [ ("hero", heroField)
        , ("human", humanField)
        , ("droid", droidField)
        ]
    heroArguments = HashMap.singleton "episode"
        $ In.Argument Nothing (In.NamedEnumType episodeEnum) Nothing
    heroField =
        Out.Field Nothing (Out.NamedObjectType heroObject) heroArguments hero
    humanArguments = HashMap.singleton "id"
        $ In.Argument Nothing (In.NonNullScalarType string) Nothing
    humanField =
        Out.Field Nothing (Out.NamedObjectType heroObject) humanArguments human
    droidField = Out.Field Nothing (Out.NamedObjectType droidObject) mempty droid

heroObject :: Out.ObjectType Identity
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
    homePlanetFieldType = Out.Field Nothing (Out.NamedScalarType string) mempty
        $ idField "homePlanet"

droidObject :: Out.ObjectType Identity
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
    primaryFunctionFieldType = Out.Field Nothing (Out.NamedScalarType string) mempty
        $ idField "primaryFunction"

typenameFieldType :: Out.Field Identity
typenameFieldType = Out.Field Nothing (Out.NamedScalarType string) mempty
    $ idField "__typename"

idFieldType :: Out.Field Identity
idFieldType = Out.Field Nothing (Out.NamedScalarType id) mempty
    $ idField "id"

nameFieldType :: Out.Field Identity
nameFieldType = Out.Field Nothing (Out.NamedScalarType string) mempty
    $ idField "name"

friendsFieldType :: Out.Field Identity
friendsFieldType = Out.Field Nothing (Out.ListType $ Out.NamedObjectType droidObject) mempty
    $ idField "friends"

appearsInField :: Out.Field Identity
appearsInField = Out.Field (Just description) fieldType mempty
    $ idField "appearsIn"
  where
    fieldType = Out.ListType $ Out.NamedEnumType episodeEnum
    description = "Which movies they appear in."

secretBackstoryFieldType :: Out.Field Identity
secretBackstoryFieldType = Out.Field Nothing (Out.NamedScalarType string) mempty
    $ String <$> secretBackstory

idField :: Text -> ResolverT Identity Value
idField f = do
    v <- ResolverT $ lift $ asks values
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

hero :: ResolverT Identity Value
hero = do
  episode <- argument "episode"
  pure $ character $ case episode of
      Enum "NEW_HOPE" -> getHero 4
      Enum "EMPIRE" -> getHero 5
      Enum "JEDI" -> getHero 6
      _ -> artoo

human :: ResolverT Identity Value
human = do
    id' <- argument "id"
    case id' of
        String i -> do
            humanCharacter <- lift $ return $ getHuman i >>= Just
            case humanCharacter of
                Nothing -> pure Null
                Just e -> pure $ character e
        _ -> ResolverT $ throwE "Invalid arguments."

droid :: ResolverT Identity Value
droid = do
    id' <- argument "id"
    case id' of
        String i -> character <$> getDroid i
        _ -> ResolverT $ throwE "Invalid arguments."

character :: Character -> Value
character char = Object $ HashMap.fromList
    [ ("id", String $ id_ char)
    , ("name", String $ name_ char)
    , ("friends", List $ character <$> getFriends char)
    , ("appearsIn", List $ Enum <$> catMaybes (getEpisode <$> appearsIn char))
    , ("homePlanet", String $ either mempty homePlanet char)
    , ("__typename", String $ typeName char)
    ]
