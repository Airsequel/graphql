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
import qualified Language.GraphQL.Type as Type
import Language.GraphQL.Type.Schema
import Test.StarWars.Data

-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: Schema Identity
schema = Schema { query = queryType, mutation = Nothing }
  where
    queryType = ObjectType "Query" Nothing $ HashMap.fromList
        [ ("hero", Field Nothing (ScalarOutputType string) mempty hero)
        , ("human", Field Nothing (ScalarOutputType string) mempty human)
        , ("droid", Field Nothing (ScalarOutputType string) mempty droid)
        ]

hero :: FieldResolver Identity
hero = NestingResolver $ do
  episode <- argument "episode"
  pure $ character $ case episode of
      Schema.Enum "NEWHOPE" -> getHero 4
      Schema.Enum "EMPIRE" -> getHero 5
      Schema.Enum "JEDI" -> getHero 6
      _ -> artoo

human :: FieldResolver Identity
human = NestingResolver $ do
    id' <- argument "id"
    case id' of
        Schema.String i -> do
            humanCharacter <- lift $ return $ getHuman i >>= Just
            case humanCharacter of
                Nothing -> pure Type.Null
                Just e -> pure $ character e
        _ -> ActionT $ throwE "Invalid arguments."

droid :: FieldResolver Identity
droid = NestingResolver $ do
    id' <- argument "id"
    case id' of
        Schema.String i -> getDroid i >>= pure . character
        _ -> ActionT $ throwE "Invalid arguments."

character :: Character -> Type.Wrapping (FieldResolver Identity)
character char = Schema.object
    [ Schema.wrappedObject "id" $ pure $ Type.S $ id_ char
    , Schema.wrappedObject "name" $ pure $ Type.S $ name_ char
    , Schema.wrappedObject "friends"
        $ pure
        $ Type.List
        $ fmap character
        $ getFriends char
    , Schema.wrappedObject "appearsIn" $ pure
        $ Type.List $ Type.E <$> catMaybes (getEpisode <$> appearsIn char)
    , Schema.wrappedObject "secretBackstory" $ Type.S <$> secretBackstory char
    , Schema.wrappedObject "homePlanet" $ pure $ Type.S $ either mempty homePlanet char
    , Schema.wrappedObject "__typename" $ pure $ Type.S $ typeName char
    ]
