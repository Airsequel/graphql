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
import Language.GraphQL.Trans
import Language.GraphQL.Type.Definition
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema
import Test.StarWars.Data
import Prelude hiding (id)

-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: Schema Identity
schema = Schema { query = queryType, mutation = Nothing }
  where
    queryType = Out.ObjectType "Query" Nothing [] $ HashMap.fromList
        [ ("hero", Out.Field Nothing (Out.NamedObjectType heroObject) mempty hero)
        , ("human", Out.Field Nothing (Out.NamedObjectType heroObject) mempty human)
        , ("droid", Out.Field Nothing (Out.NamedObjectType droidObject) mempty droid)
        ]

heroObject :: Out.ObjectType Identity
heroObject = Out.ObjectType "Human" Nothing [] $ HashMap.fromList
    [ ("id", Out.Field Nothing (Out.NamedScalarType id) mempty (idField "id"))
    , ("name", Out.Field Nothing (Out.NamedScalarType string) mempty (idField "name"))
    , ("friends", Out.Field Nothing (Out.ListType $ Out.NamedObjectType heroObject) mempty (idField "friends"))
    , ("appearsIn", Out.Field Nothing (Out.ListType $ Out.NamedScalarType int) mempty (idField "appearsIn"))
    , ("homePlanet", Out.Field Nothing (Out.NamedScalarType string) mempty (idField "homePlanet"))
    , ("secretBackstory", Out.Field Nothing (Out.NamedScalarType string) mempty (String <$> secretBackstory))
    , ("__typename", Out.Field Nothing (Out.NamedScalarType string) mempty (idField "__typename"))
    ]

droidObject :: Out.ObjectType Identity
droidObject = Out.ObjectType "Droid" Nothing [] $ HashMap.fromList
    [ ("id", Out.Field Nothing (Out.NamedScalarType id) mempty (idField "id"))
    , ("name", Out.Field Nothing (Out.NamedScalarType string) mempty (idField "name"))
    , ("friends", Out.Field Nothing (Out.ListType $ Out.NamedObjectType droidObject) mempty (idField "friends"))
    , ("appearsIn", Out.Field Nothing (Out.ListType $ Out.NamedScalarType int) mempty (idField "appearsIn"))
    , ("primaryFunction", Out.Field Nothing (Out.NamedScalarType string) mempty (idField "primaryFunction"))
    , ("secretBackstory", Out.Field Nothing (Out.NamedScalarType string) mempty (String <$> secretBackstory))
    , ("__typename", Out.Field Nothing (Out.NamedScalarType string) mempty (idField "__typename"))
    ]

idField :: Text -> ActionT Identity Value
idField f = do
    v <- ActionT $ lift $ asks values
    let (Object v') = v
    pure $ v' HashMap.! f

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
