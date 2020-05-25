{-# LANGUAGE OverloadedStrings #-}
module Test.StarWars.Schema
    ( character
    , droid
    , hero
    , human
    , schema
    ) where

import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (catMaybes)
import qualified Language.GraphQL.Schema as Schema
import Language.GraphQL.Trans
import Language.GraphQL.Type.Definition
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema
import Test.StarWars.Data

-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: Schema Identity
schema = Schema { query = queryType, mutation = Nothing }
  where
    queryType = Out.ObjectType "Query" Nothing $ HashMap.fromList
        [ ("hero", Out.Field Nothing (Out.NamedScalarType string) mempty hero)
        , ("human", Out.Field Nothing (Out.NamedScalarType string) mempty human)
        , ("droid", Out.Field Nothing (Out.NamedScalarType string) mempty droid)
        ]

hero :: ActionT Identity (Out.Value Identity)
hero = do
  episode <- argument "episode"
  pure $ character $ case episode of
      In.Enum "NEWHOPE" -> getHero 4
      In.Enum "EMPIRE" -> getHero 5
      In.Enum "JEDI" -> getHero 6
      _ -> artoo

human :: ActionT Identity (Out.Value Identity)
human = do
    id' <- argument "id"
    case id' of
        In.String i -> do
            humanCharacter <- lift $ return $ getHuman i >>= Just
            case humanCharacter of
                Nothing -> pure Out.Null
                Just e -> pure $ character e
        _ -> ActionT $ throwE "Invalid arguments."

droid :: ActionT Identity (Out.Value Identity)
droid = do
    id' <- argument "id"
    case id' of
        In.String i -> character <$> getDroid i
        _ -> ActionT $ throwE "Invalid arguments."

character :: Character -> Out.Value Identity
character char = Schema.object
    [ Schema.Resolver "id" $ pure $ Out.String $ id_ char
    , Schema.Resolver "name" $ pure $ Out.String $ name_ char
    , Schema.Resolver "friends"
        $ pure $ Out.List $ character <$> getFriends char
    , Schema.Resolver "appearsIn" $ pure
        $ Out.List $ Out.Enum <$> catMaybes (getEpisode <$> appearsIn char)
    , Schema.Resolver "secretBackstory" $ Out.String
        <$> secretBackstory char
    , Schema.Resolver "homePlanet" $ pure $ Out.String
        $ either mempty homePlanet char
    , Schema.Resolver "__typename" $ pure $ Out.String
        $ typeName char
    ]
