{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.IO.Class (MonadIO(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes)
import qualified Language.GraphQL.Schema as Schema
import Language.GraphQL.Trans
import Language.GraphQL.Type
import Test.StarWars.Data

-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: MonadIO m => NonEmpty (Schema.Resolver m)
schema = hero :| [human, droid]

hero :: MonadIO m => Schema.Resolver m
hero = Schema.objectA "hero" $ \case
  [] -> character artoo
  [Schema.Argument "episode" (Schema.ValueEnum "NEWHOPE")] -> character $ getHero 4
  [Schema.Argument "episode" (Schema.ValueEnum "EMPIRE" )] -> character $ getHero 5
  [Schema.Argument "episode" (Schema.ValueEnum "JEDI"   )] -> character $ getHero 6
  _ -> ActionT $ throwE "Invalid arguments."

human :: MonadIO m => Schema.Resolver m
human = Schema.wrappedObjectA "human" $ \case
  [Schema.Argument "id" (Schema.ValueString i)] -> do
      humanCharacter <- lift $ return $ getHuman i >>= Just
      case humanCharacter of
        Nothing -> return Null
        Just e -> Named <$> character e
  _ -> ActionT $ throwE "Invalid arguments."

droid :: MonadIO m => Schema.Resolver m
droid = Schema.objectA "droid" $ \case
   [Schema.Argument "id" (Schema.ValueString i)] -> character =<< liftIO (getDroid i)
   _ -> ActionT $ throwE "Invalid arguments."

character :: MonadIO m => Character -> ActionT m [Schema.Resolver m]
character char = return
    [ Schema.scalar "id" $ return $ id_ char
    , Schema.scalar "name" $ return $ name char
    , Schema.wrappedObject "friends"
        $ traverse character $ List $ Named <$> getFriends char
    , Schema.wrappedScalar "appearsIn" $ return . List
        $ catMaybes (getEpisode <$> appearsIn char)
    , Schema.scalar "secretBackstory" $ secretBackstory char
    , Schema.scalar "homePlanet" $ return $ either mempty homePlanet char
    , Schema.scalar "__typename" $ return $ typeName char
    ]
