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
import qualified Data.Set as Set
import Language.GraphQL.Trans
import Language.GraphQL.Type.Definition
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema (Schema(..))
import Test.StarWars.Data
import Prelude hiding (id)

-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: Schema Identity
schema = Schema { query = queryType, mutation = Nothing }
  where
    queryType = Out.ObjectType "Query" Nothing [] $ HashMap.fromList
        [ ("hero", Out.Resolver heroField hero)
        , ("human", Out.Resolver humanField human)
        , ("droid", Out.Resolver droidField droid)
        ]
    heroField = Out.Field Nothing (Out.NamedObjectType heroObject)
        $ HashMap.singleton "episode"
        $ In.Argument Nothing (In.NamedEnumType episodeEnum) Nothing
    humanField = Out.Field Nothing (Out.NamedObjectType heroObject)
        $ HashMap.singleton "id"
        $ In.Argument Nothing (In.NonNullScalarType string) Nothing
    droidField = Out.Field Nothing (Out.NamedObjectType droidObject) mempty

heroObject :: Out.ObjectType Identity
heroObject = Out.ObjectType "Human" Nothing [] $ HashMap.fromList
    [ ("id", Out.Resolver idFieldType (idField "id"))
    , ("name", Out.Resolver nameFieldType (idField "name"))
    , ("friends", Out.Resolver friendsFieldType (idField "friends"))
    , ("appearsIn", Out.Resolver appearsInFieldType (idField "appearsIn"))
    , ("homePlanet", Out.Resolver homePlanetFieldType (idField "homePlanet"))
    , ("secretBackstory", Out.Resolver secretBackstoryFieldType (String <$> secretBackstory))
    , ("__typename", Out.Resolver (Out.Field Nothing (Out.NamedScalarType string) mempty) (idField "__typename"))
    ]
  where
    homePlanetFieldType = Out.Field Nothing (Out.NamedScalarType string) mempty

droidObject :: Out.ObjectType Identity
droidObject = Out.ObjectType "Droid" Nothing [] $ HashMap.fromList
    [ ("id", Out.Resolver idFieldType (idField "id"))
    , ("name", Out.Resolver nameFieldType (idField "name"))
    , ("friends", Out.Resolver friendsFieldType (idField "friends"))
    , ("appearsIn", Out.Resolver appearsInFieldType (idField "appearsIn"))
    , ("primaryFunction", Out.Resolver primaryFunctionFieldType (idField "primaryFunction"))
    , ("secretBackstory", Out.Resolver secretBackstoryFieldType (String <$> secretBackstory))
    , ("__typename", Out.Resolver (Out.Field Nothing (Out.NamedScalarType string) mempty) (idField "__typename"))
    ]
  where
    primaryFunctionFieldType = Out.Field Nothing (Out.NamedScalarType string) mempty

idFieldType :: Out.Field Identity
idFieldType = Out.Field Nothing (Out.NamedScalarType id) mempty

nameFieldType :: Out.Field Identity
nameFieldType = Out.Field Nothing (Out.NamedScalarType string) mempty

friendsFieldType :: Out.Field Identity
friendsFieldType = Out.Field Nothing (Out.ListType $ Out.NamedObjectType droidObject) mempty

appearsInFieldType :: Out.Field Identity
appearsInFieldType = Out.Field Nothing (Out.ListType $ Out.NamedScalarType int) mempty

secretBackstoryFieldType :: Out.Field Identity
secretBackstoryFieldType = Out.Field Nothing (Out.NamedScalarType string) mempty

idField :: Text -> ActionT Identity Value
idField f = do
    v <- ActionT $ lift $ asks values
    let (Object v') = v
    pure $ v' HashMap.! f

episodeEnum :: EnumType
episodeEnum = EnumType "Episode" Nothing
    $ Set.fromList ["NEW_HOPE", "EMPIRE", "JEDI"]

hero :: ActionT Identity Value
hero = do
  episode <- argument "episode"
  pure $ character $ case episode of
      Enum "NEWHOPE" -> getHero 4
      Enum "EMPIRE" -> getHero 5
      Enum "JEDI" -> getHero 6
      _ -> artoo

human :: ActionT Identity Value
human = do
    id' <- argument "id"
    case id' of
        String i -> do
            humanCharacter <- lift $ return $ getHuman i >>= Just
            case humanCharacter of
                Nothing -> pure Null
                Just e -> pure $ character e
        _ -> ActionT $ throwE "Invalid arguments."

droid :: ActionT Identity Value
droid = do
    id' <- argument "id"
    case id' of
        String i -> character <$> getDroid i
        _ -> ActionT $ throwE "Invalid arguments."

character :: Character -> Value
character char = Object $ HashMap.fromList
    [ ("id", String $ id_ char)
    , ("name", String $ name_ char)
    , ("friends", List $ character <$> getFriends char)
    , ("appearsIn", List $ Enum <$> catMaybes (getEpisode <$> appearsIn char))
    , ("homePlanet", String $ either mempty homePlanet char)
    , ("__typename", String $ typeName char)
    ]
